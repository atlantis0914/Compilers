module Compile.Backend.LivePredicates where

import Compile.Types
import Data.List
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

type LiveMap = Map.Map Int [ALoc]
type LabelMap = Map.Map Int Int
type LiveContext = (LiveMap, Bool)

extractLocs :: [AVal] -> [ALoc]
extractLocs [] = []
extractLocs (val:vals) =
  case val of ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

getLocs :: Int -> LiveMap -> [ALoc]
getLocs i liveMap =
  case Map.lookup i liveMap of Nothing -> []
                               Just locs -> locs

addLocs :: Int -> LiveContext -> [ALoc] -> LiveContext
addLocs i (liveMap, isNew) locs =
  let
    maybeOldLocs = Map.lookup i liveMap
    oldLocs = case maybeOldLocs of Nothing -> error "OLD LOC NOT FOUND"
                                   Just l -> l
    isNew' = (length oldLocs) /= (length locs)
    liveMap' = Map.insert i locs liveMap
  in
    (liveMap', isNew || isNew')

runPredicate :: LabelMap -> Int -> AAsm -> LiveContext -> LiveContext
runPredicate _ i (AAsm {aAssign = [AReg 0], aArgs = args}) (liveMap, isNew) =
  let
    locs = nub $ extractLocs args
  in
    addLocs i (liveMap, isNew) locs

runPredicate _ i (AAsm {aAssign = assigns, aArgs = args}) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns)
  in
    addLocs i (liveMap, isNew) locs'

runPredicate _ i (ACtrl (ARet _)) liveCtx = liveCtx

runPredicate _ i (ACtrl (ALabel _)) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
  in
    addLocs i (liveMap, isNew) locs

runPredicate labelMap i (ACtrl (AIf aval label)) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
    locs' = locs `union` (labelLocs labelMap liveMap label)
    locs'' = case aval of ALoc loc -> locs' `union` [loc]
                          _ -> locs'
  in
    addLocs i (liveMap, isNew) locs''

runPredicate labelMap i (ACtrl (AGoto label)) (liveMap, isNew) =
  let
    locs = labelLocs labelMap liveMap label
  in
    addLocs i (liveMap, isNew) locs

labelLocs :: LabelMap -> LiveMap -> Int -> [ALoc]
labelLocs labelMap liveMap label =
  let
    i = case Map.lookup label labelMap of Just i' -> i'
                                          Nothing -> error ("NO LABEL " ++ show label)
                    
  in
    case Map.lookup i liveMap of Nothing -> error ("NO LINE " ++ show i)
                                 Just locs -> locs

