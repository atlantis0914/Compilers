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

deadCodeOptLevel :: Int
deadCodeOptLevel = 2

regCoalesceOptLevel :: Int
regCoalesceOptLevel = 2

labelCoalesceOptLevel :: Int
labelCoalesceOptLevel = 2

squashOptLevel :: Int
squashOptLevel = 2
