module Compile.Backend.Neededness where

import Compile.Types
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Tuple as Tuple

type NeedMap = Map.Map Int [ALoc]
type LabelMap = Map.Map Int Int
type NeedContext = (NeedMap, Bool)

removeDead :: [AAsm] -> [AAsm]
removeDead aasms =
  let
    needs = neededness aasms
    pairs = zip (take ((length aasms) - 1) aasms) (drop 1 needs)
  in
    (map fst (filter removeDeadLine pairs)) ++ [last aasms]

removeDeadLine :: (AAsm, [ALoc]) -> Bool
removeDeadLine (AAsm _ Div _, _) = True
removeDeadLine (AAsm _ Mod _, _) = True
removeDeadLine (AAsm [AReg 0 _] op _, needs) = True
removeDeadLine (AAsm [APtr _ _ _ _ _] op _, needs) = True
removeDeadLine (AAsm [ASpill _] op _, needs) = True
removeDeadLine (AAsm [AUtil _] op _, needs) = True
removeDeadLine (AAsm [AIndex _] op _, needs) = True
removeDeadLine (AAsm [AArg _ _] op _, needs) = True
removeDeadLine (AAsm [AMem _ _] op _, needs) = True
removeDeadLine (AAsm [ATemp i b] op _, needs) = (ATemp i b) `elem` needs
removeDeadLine (_, _) = True

neededness :: [AAsm] -> [[ALoc]]
neededness aasms =
  let
    aasmsSeq = Seq.fromList aasms
    (labelMap, initial) = Seq.foldlWithIndex (addLabel) (Map.empty, Map.empty) aasmsSeq
    needMap = neededness' aasmsSeq labelMap initial
  in
    Map.elems needMap

neededness' :: Seq.Seq AAsm -> LabelMap -> NeedMap -> NeedMap
neededness' aasmsSeq labelMap needMap =
  let
    (needMap', isNew) = Seq.foldrWithIndex (runNeedPred labelMap) (needMap, False) aasmsSeq
  in
    if isNew then neededness' aasmsSeq labelMap needMap'
             else needMap'

addLabel :: (LabelMap, NeedMap) -> Int -> AAsm -> (LabelMap, NeedMap)
addLabel (labelMap, needMap) i (ACtrl (ALabel label)) =
  (Map.insert label i labelMap, Map.insert i [] needMap)
addLabel (labelMap, needMap) i _ = (labelMap, Map.insert i [] needMap)

extractLocs :: [AVal] -> [ALoc]
extractLocs [] = []
extractLocs (val:vals) =
  case val of ALoc (APtr base index _ _ _) -> (case index of Nothing -> base : extractLocs vals
                                                             Just loc -> loc : base : extractLocs vals)
              ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

getPredLocs :: Int -> NeedMap -> [ALoc]
getPredLocs i needMap =
  case Map.lookup i needMap of Nothing -> []
                               Just locs -> locs

addLocs :: Int -> NeedContext -> [ALoc] -> NeedContext
addLocs i (needMap, isNew) locs =
  let
    locs' = filterPredLocs locs
    maybeOldLocs = Map.lookup i needMap
    oldLocs = case maybeOldLocs of Nothing -> error "OLD LOC NOT FOUND"
                                   Just l -> l
    isNew' = (length oldLocs) /= (length locs')
    needMap' = Map.insert i locs' needMap
  in
    (needMap', isNew || isNew')

runNeedPred :: LabelMap -> Int -> AAsm -> NeedContext -> NeedContext
runNeedPred _ i (AAsm [AReg _ _] op args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    locs' = (nub $ extractLocs args) `union` locs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm [AIndex _] op args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    locs' = (nub $ extractLocs args) `union` locs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm [ASpill _] op args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    locs' = (nub $ extractLocs args) `union` locs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm [AUtil _] op args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    locs' = (nub $ extractLocs args) `union` locs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm [APtr loc _ _ _ _] op args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    locs' = (nub $ extractLocs args) `union` locs `union` [loc]
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm assigns Div args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    assigns' = filterOutPtrs assigns
    ptrs = filterPtrs assigns
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns') `union` ptrs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm assigns Mod args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    assigns' = filterOutPtrs assigns
    ptrs = filterPtrs assigns
    locs' = (nub $ extractLocs args) `union` (locs \\ assigns') `union` ptrs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (AAsm [loc] op args) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
    locNeeded = loc `elem` locs
    locs' = if locNeeded
              then (nub $ extractLocs args) `union` (locs \\ [loc])
              else locs
  in
    addLocs i (needMap, isNew) locs'

runNeedPred _ i (ACtrl (ARet _)) needCtx = needCtx

runNeedPred _ i (ACtrl (ALabel _)) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
  in
    addLocs i (needMap, isNew) locs

runNeedPred labelMap i (ACtrl (AIf aval label Nothing opt)) (needMap, isNew) = let
    locs = getPredLocs (i+1) needMap
    locs' = locs `union` (labelLocs labelMap needMap label)
    locs'' = case aval of ALoc loc -> locs' `union` [loc]
                          _ -> locs'
  in
    addLocs i (needMap, isNew) locs''

runNeedPred labelMap i (ACtrl (AIf aval label (Just e) opt)) (needMap, isNew) =
  let
    locs = getPredLocs (i+1) needMap
  in
    addLocs i (needMap, isNew) locs

runNeedPred labelMap i (ACtrl (AGoto label)) (needMap, isNew) =
  let
    locs = labelLocs labelMap needMap label
  in
    addLocs i (needMap, isNew) locs

runNeedPred labelMap i (AFnCall _ loc locs _) (needMap, isNew) =
  let
    locs' = (getPredLocs (i+1) needMap) \\ [loc]
    locs'' = locs' `union` locs
  in
    addLocs i (needMap, isNew) locs''

isPredTemp :: ALoc -> Bool
isPredTemp aloc = case aloc of ATemp _ _ -> True
                               AReg _ _ -> True
                               _ -> False

isPredPtr :: ALoc -> Bool
isPredPtr loc = case loc of APtr _ _ _ _ _ -> True
                            _ -> False

filterPredLocs :: [ALoc] -> [ALoc]
filterPredLocs locs = filter isPredTemp locs

extractPredLocsFromPtr (APtr base index _ _ _) =
  let
    index' = case index of Nothing -> []
                           Just loc -> [loc]
  in
    index' ++ [base]

filterPtrs :: [ALoc] -> [ALoc]
filterPtrs locs = concatMap extractPredLocsFromPtr (filter isPredPtr locs)

filterOutPtrs :: [ALoc] -> [ALoc]
filterOutPtrs locs = filter (\loc -> not $ isPredPtr loc) locs

labelLocs :: LabelMap -> NeedMap -> Int -> [ALoc]
labelLocs labelMap needMap label =
  let
    i = case Map.lookup label labelMap of Just i' -> i'
                                          Nothing -> error ("NO LABEL " ++ show label)
  in
    case Map.lookup i needMap of Nothing -> error ("NO LINE " ++ show i)
                                 Just locs -> locs

