
module Compile.Backend.Liveness where

import Compile.Types
import Compile.Backend.LivePredicates


liveness :: [AAsm] -> [[ALoc]]
liveness aasms = scanr runPredicate [] aasms
