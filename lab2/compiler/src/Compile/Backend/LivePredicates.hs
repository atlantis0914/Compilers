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

runPredicate :: LabelMap -> Int -> AAsm -> LiveContext -> LiveContext
runPredicate _ i (AAsm {aAssign = [AReg 0], aArgs = args}) (liveMap, isNew) =
  let
    oldLocs = liveMap Map.! i
    locs = nub $ extractLocs args
    isNew' = (length oldLocs) == (length locs)
    liveMap' = Map.insert i locs liveMap
  in
    (liveMap', isNew || isNew')

runPredicate _ i (AAsm {aAssign = assigns, aArgs = args}) (liveMap, isNew) =
  let
    locs = liveMap Map.! (i+1)
    oldLocs = liveMap Map.! i
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns)
    isNew' = (length oldLocs) == (length locs')
    liveMap' = Map.insert i locs' liveMap
  in
    (liveMap', isNew || isNew')

runPredicate _ i (ACtrl (ALabel _)) (liveMap, isNew) =
  let
    oldLocs = liveMap Map.! i
    locs = liveMap Map.! (i+1)
    liveMap' = Map.insert i locs liveMap
    isNew' = (length oldLocs) == (length locs)
  in
    (liveMap', isNew || isNew')

runPredicate labelMap i (ACtrl (AIf aval label)) (liveMap, isNew) =
  let
    oldLocs = liveMap Map.! i
    locs = liveMap Map.! (i+1)
    labelIndex = labelMap Map.! label
    locs' = locs `union` (liveMap Map.! labelIndex)
    liveMap' = Map.insert i locs' liveMap
    isNew' = (length oldLocs) == (length locs')
  in
    (liveMap', isNew || isNew')

runPredicate labelMap i (ACtrl (AGoto label)) (liveMap, isNew) =
  let
    oldLocs = liveMap Map.! i
    labelIndex = labelMap Map.! label
    locs = liveMap Map.! labelIndex
    liveMap' = Map.insert i locs liveMap
    isNew' = (length oldLocs) == (length locs)
  in
    (liveMap', isNew || isNew')
