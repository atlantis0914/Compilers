module Compile.Backend.LivePredicates where

import Compile.Types
import Data.List
import qualified Data.Map as Map

type LiveMap = Data.Map Int [ALoc]
type LabelMap = Data.Map Int Int

extractLocs :: [AVal] -> [ALoc]
extractLocs [] = []
extractLocs (val:vals) =
  case val of ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

runPredicate :: LabelMap -> Int -> AAsm -> LiveMap -> LiveMap
runPredicate labelMap i (AAsm {aAssign = [AReg 0], aArgs = args}) liveMap =
  let
    locs = nub $ extractLocs args
    liveMap' = Map.insert i locs liveMap
  in
    liveMap'

runPredicate labelMap i (AAsm {aAssign = assigns, aArgs = args}) liveMap =
  let
    locs = Map.! (i+1) liveMap
    oldLocs = Map.! i liveMap
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns)
    liveMap' = Map.insert i locs' liveMap
  in
    liveMap'

runPredicate labelMap i (ACtrl (Label _)) liveMap =
  let
    locs = Map.! (i+1) liveMap
    liveMap' = Map.insert i locs liveMap
  in
    liveMap'

runPredicate labelMap i (ACtrl (If aval label)) liveMap =
  let
    locs = Map.! (i+1) liveMap
    labelIndex = Map.! label labelMap
    locs' = locs `union` (Map.!) labelIndex liveMap
    liveMap' = Map.insert i locs' liveMap
  in
    liveMap'

runPredicate labelMap i (ACtrl (Goto i)) liveMap =
  let
    labelIndex = Map.! label labelMap
    locs = Map.! labelIndex liveMap
    liveMap' = Map.insert i locs' liveMap
  in
    liveMap'
