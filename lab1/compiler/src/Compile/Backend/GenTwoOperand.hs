module Compile.Backend.GenTwoOperand where

import Compile.Types
import Compile.Util.Graph
import Compile.Util.AbstractAssembly

import Data.List
import qualified Data.Map as Map

genTwoOperand :: [AAsm] -> [AAsm]
genTwoOperand aasmList = concat $ map genForIns aasmList
   
-- Generates the 2operand AAsm for the given instruction
genForIns :: AAsm -> [AAsm]
genForIns aasm@(AAsm {aAssign = locs, aOp = Nop, aArgs = [fst]}) = [aasm]
-- Transform d <- c + s2 into d <- c, d <- d + s2
genForIns (AAsm {aAssign = locs, aOp = op, aArgs = [AImm c, snd]}) = 
  [AAsm {aAssign = locs, aOp = Nop, aArgs = [AImm c]}, 
   AAsm {aAssign = locs, aOp = op, aArgs = [ALoc (head locs), snd]}]

genForIns (AAsm {aAssign = locs, aOp = op, aArgs = [fst, AImm c]}) = 
  [AAsm {aAssign = locs, aOp = Nop, aArgs = [AImm c]}, 
   AAsm {aAssign = locs, aOp = op, aArgs = [fst, ALoc (head locs)]}]

-- Note that this will convert d <- s1 + s1 into d <- s1, d <- d + s1 
genForIns (AAsm {aAssign = locs, aOp = op, aArgs = [s1, s2]}) = 
  [AAsm {aAssign = locs, aOp = Nop, aArgs = [s1]},
   AAsm {aAssign = locs, aOp = op, aArgs = [ALoc (head locs), s2]}]
