module Compile.Util.Job where 

import Compile.Types
import qualified Data.Map as Map
import Data.List

-- Creates a new ColoringMap where each location points to Uncolored

optLevelMet :: Job -> Int -> Bool
optLevelMet (Job {jobOptimization = optLevel}) i = (optLevel >= i)

inliningOptLevel :: Int
inliningOptLevel = 2

constantPropOptLevel :: Int
constantPropOptLevel = 1
