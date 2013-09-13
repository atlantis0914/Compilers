
module Compile.Backend.Liveness where

import Compile.Types
import Compile.Backend.LivePredicates
import Data.List

liveness :: [AAsm] -> [[ALoc]]
liveness aasms = take (length aasms) (scanr runPredicate [] aasms) 
