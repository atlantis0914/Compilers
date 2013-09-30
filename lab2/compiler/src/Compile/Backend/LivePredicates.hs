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
    oldLocs = liveMap Map.! i
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

runPredicate _ i (ACtrl (ALabel _)) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
  in
    addLocs i (liveMap, isNew) locs

runPredicate labelMap i (ACtrl (AIf aval label)) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
    labelIndex = labelMap Map.! label
    locs' = locs `union` (liveMap Map.! labelIndex)
    locs'' = case aval of ALoc loc -> locs' `union` [loc]
                          _ -> locs'
  in
    addLocs i (liveMap, isNew) locs''

runPredicate labelMap i (ACtrl (AGoto label)) (liveMap, isNew) =
  let
    labelIndex = labelMap Map.! label
    locs = liveMap Map.! labelIndex
  in
    addLocs i (liveMap, isNew) locs
