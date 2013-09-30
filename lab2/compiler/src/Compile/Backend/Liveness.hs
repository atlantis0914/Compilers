
module Compile.Backend.Liveness where

import Compile.Types
import Compile.Backend.LivePredicates
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Tuple as Tuple

liveness :: [AAsm] -> [[ALoc]]
liveness aasms =
  let
    initial = take (length aasms) (iterate (\x -> x + 1) 1)
    initial' = map (\n -> (n, [])) initial
    initial'' = Map.fromList initial'
    aasmsSeq = Seq.fromList aasms
    labelMap = Seq.foldlWithIndex (addLabel) (Map.empty) aasmsSeq
    liveMap = liveness' aasms labelMap initial''
  in
    map (\i -> liveMap Map.! i ) initial

liveness' :: [AAsm] -> LabelMap -> LiveMap -> LiveMap
liveness' aasms labelMap liveMap =
  let
    aasmsSeq = Seq.fromList aasms
    (liveMap', isNew) = Seq.foldrWithIndex (runPredicate labelMap) (liveMap, False) aasmsSeq
  in
    if isNew then liveness' aasms labelMap liveMap'
             else liveMap'

addLabel :: LabelMap -> Int -> AAsm -> LabelMap
addLabel labelMap i (ACtrl (ALabel label)) =
  Map.insert label i labelMap
addLabel labelMap _ _ = labelMap
