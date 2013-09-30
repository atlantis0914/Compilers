module Compile.Backend.LivePredicates where

import Compile.Types
import Data.List
import qualified Data.Map as Map

type LiveMap = Map.Map Int [ALoc]
type LabelMap = Map.Map Int Int

extractLocs :: [AVal] -> [ALoc]
extractLocs [] = []
extractLocs (val:vals) =
  case val of ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

runPredicate :: LabelMap -> Int -> AAsm -> LiveMap -> LiveMap
runPredicate _ i (AAsm {aAssign = [AReg 0], aArgs = args}) liveMap =
  let
    locs = nub $ extractLocs args
    liveMap' = Map.insert i locs liveMap
  in
    liveMap'

runPredicate _ i (AAsm {aAssign = assigns, aArgs = args}) liveMap =
  let
    locs = liveMap Map.! (i+1)
    oldLocs = liveMap Map.! i
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns)
    liveMap' = Map.insert i locs' liveMap
  in
    liveMap'

runPredicate _ i (ACtrl (ALabel _)) liveMap =
  let
    locs = liveMap Map.! (i+1)
    liveMap' = Map.insert i locs liveMap
  in
    liveMap'

runPredicate labelMap i (ACtrl (AIf aval label)) liveMap =
  let
    locs = liveMap Map.! (i+1)
    labelIndex = labelMap Map.! label
    locs' = locs `union` (liveMap Map.! labelIndex)
    liveMap' = Map.insert i locs' liveMap
  in
    liveMap'

runPredicate labelMap i (ACtrl (AGoto label)) liveMap =
  let
    labelIndex = labelMap Map.! label
    locs = liveMap Map.! labelIndex
    liveMap' = Map.insert i locs liveMap
  in
    liveMap'
