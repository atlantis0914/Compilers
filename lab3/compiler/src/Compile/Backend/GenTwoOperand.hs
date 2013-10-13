module Compile.Backend.GenTwoOperand where

import Compile.Types
import Compile.Util.Graph
import Compile.Util.AbstractAssembly

import Data.List
import qualified Data.Map as Map

import Debug.Trace as Trace

genTwoOperand :: [AAsm] -> [AAsm]
genTwoOperand aasmList = concat $ map genForIns aasmList

-- Generates the 2operand AAsm for the given instruction
genForIns :: AAsm -> [AAsm]
genForIns aasm@(AAsm {aAssign = locs, aOp = Nop, aArgs = [fst]}) = [aasm]
genForIns aasm@(AAsm {aAssign = [loc], aOp = op, aArgs = [fst,snd]}) = genForIns' aasm
-- Generates expansion for unary operators
genForIns aasm@(AAsm {aAssign = [loc], aOp = op, aArgs = [fst]}) =
  if ALoc loc == fst
    then [aasm]
    else [AAsm {aAssign = [loc], aOp = Nop, aArgs = [fst]},
          AAsm {aAssign = [loc], aOp = op, aArgs = [ALoc loc]}]


genForIns aasm@(ACtrl c) = [aasm]

genForIns aasm = [aasm]

genForIns' aasm@(AAsm {aAssign = loc:locs, aOp = op, aArgs = [fst, snd]}) =
  [AAsm {aAssign = loc:locs, aOp = Nop, aArgs = [fst]},
   AAsm {aAssign = loc:locs, aOp = op, aArgs = [snd]}]

genForDiv aasm@(AAsm {aAssign = loc:locs, aOp = op, aArgs = [fst, snd]}) =
  [AAsm {aAssign = loc:locs, aOp = Nop, aArgs = [fst]},
   AAsm {aAssign = loc:locs, aOp = op, aArgs = [ALoc loc, snd]}]


