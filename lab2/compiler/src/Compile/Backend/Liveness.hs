
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
    initial'' = Map.fromList initial''

    liveMap = foldrWithIndex runPredicate initial'' (Seq.fromList aasms)
  in
    map (\i -> Map.! i liveMap) initial
