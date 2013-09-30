
module Compile.Backend.Liveness where

import Compile.Types
import Compile.Backend.LivePredicates
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

liveness :: [AAsm] -> [[ALoc]]
liveness aasms =
  let
    initial = take (length aasms) (iterate (\x -> x + 1) 1)
    initial' = map (\n -> (n, [])) initial
    initial'' = Map.fromList initial'
    aasmsSeq = Seq.fromList aasms
    labelMap = Seq.foldlWithIndex (addLabel) (Map.empty) aasmsSeq
    liveMap = Seq.foldrWithIndex (runPredicate labelMap) initial'' aasmsSeq
  in
    map (\i -> liveMap Map.! i ) initial

addLabel :: LabelMap -> Int -> AAsm -> LabelMap
addLabel labelMap i (ACtrl (ALabel label)) =
  Map.insert label i labelMap
addLabel labelMap _ _ = labelMap
