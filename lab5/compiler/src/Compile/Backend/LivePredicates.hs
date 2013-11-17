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
  case val of ALoc (APtr base index _ _ _) -> (case index of Nothing -> base : extractLocs vals
                                                             Just loc -> loc : base : extractLocs vals)
              ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

getLocs :: Int -> LiveMap -> [ALoc]
getLocs i liveMap =
  case Map.lookup i liveMap of Nothing -> []
                               Just locs -> locs

addLocs :: Int -> LiveContext -> [ALoc] -> LiveContext
addLocs i (liveMap, isNew) locs =
  let
    locs' = filterLocs locs
    maybeOldLocs = Map.lookup i liveMap
    oldLocs = case maybeOldLocs of Nothing -> error "OLD LOC NOT FOUND"
                                   Just l -> l
    isNew' = (length oldLocs) /= (length locs')
    liveMap' = Map.insert i locs' liveMap
  in
    (liveMap', isNew || isNew')

runPredicate :: LabelMap -> Int -> AAsm -> LiveContext -> LiveContext
runPredicate _ i (AAsm {aAssign = [AReg 0 _], aArgs = args}) (liveMap, isNew) =
  let
    locs = nub $ extractLocs args
  in
    addLocs i (liveMap, isNew) locs

runPredicate _ i (AAsm {aAssign = assigns, aArgs = args}) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
    assigns' = filterOutPtrs assigns
    ptrs = filterPtrs assigns
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns') `union` ptrs
  in
    addLocs i (liveMap, isNew) locs'

runPredicate _ i (ACtrl (ARet _)) liveCtx = liveCtx

runPredicate _ i (ACtrl (ALabel _)) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
  in
    addLocs i (liveMap, isNew) locs

runPredicate labelMap i (ACtrl (AIf aval label Nothing _)) (liveMap, isNew) = let
    locs = getLocs (i+1) liveMap
    locs' = locs `union` (labelLocs labelMap liveMap label)
    locs'' = case aval of ALoc loc -> locs' `union` [loc]
                          _ -> locs'
  in
    addLocs i (liveMap, isNew) locs''

runPredicate labelMap i (ACtrl (AIf aval label (Just e) _)) (liveMap, isNew) =
  let
    locs = getLocs (i+1) liveMap
  in
    addLocs i (liveMap, isNew) locs

runPredicate labelMap i (ACtrl (AGoto label)) (liveMap, isNew) =
  let
    locs = labelLocs labelMap liveMap label
  in
    addLocs i (liveMap, isNew) locs

runPredicate labelMap i (AFnCall _ loc locs _) (liveMap, isNew) =
  let
    locs' = (getLocs (i+1) liveMap) \\ [loc]
    locs'' = locs' `union` locs
  in
    addLocs i (liveMap, isNew) locs''

isTemp :: ALoc -> Bool
isTemp aloc = case aloc of ATemp _ _ -> True
                           AReg _ _ -> True
                           _ -> False

isPtr :: ALoc -> Bool
isPtr loc = case loc of APtr _ _ _ _ _ -> True
                        _ -> False

filterLocs :: [ALoc] -> [ALoc]
filterLocs locs = filter isTemp locs

extractLocsFromPtr (APtr base index _ _ _) =
  let
    index' = case index of Nothing -> []
                           Just loc -> [loc]
  in
    index' ++ [base]

filterPtrs :: [ALoc] -> [ALoc]
filterPtrs locs = concatMap extractLocsFromPtr (filter isPtr locs)

filterOutPtrs :: [ALoc] -> [ALoc]
filterOutPtrs locs = filter (\loc -> not $ isPtr loc) locs

labelLocs :: LabelMap -> LiveMap -> Int -> [ALoc]
labelLocs labelMap liveMap label =
  let
    i = case Map.lookup label labelMap of Just i' -> i'
                                          Nothing -> error ("NO LABEL " ++ show label)
                    
  in
    case Map.lookup i liveMap of Nothing -> error ("NO LINE " ++ show i)
                                 Just locs -> locs

