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
genForIns aasm@(AAsm {aAssign = [loc], aOp = Neg, aArgs = [fst]}) =
  if ALoc loc == fst
    then [aasm]
    else [AAsm {aAssign = [loc], aOp = Nop, aArgs = [fst]},
          AAsm {aAssign = [loc], aOp = Neg, aArgs = [ALoc loc]}]
genForIns aasm@(AAsm {aAssign = [loc], aOp = op, aArgs = [fst,snd]})
  | ALoc loc == fst = [AAsm {aAssign = [loc], aOp = op, aArgs = [snd]}]
  | ALoc loc == snd = [AAsm {aAssign = [loc], aOp = op, aArgs = [fst]}]
  | otherwise = genForIns' aasm

genForIns' aasm@(AAsm {aAssign = loc:locs, aOp = op, aArgs = [fst, snd]}) =
  [AAsm {aAssign = loc:locs, aOp = Nop, aArgs = [fst]},
   AAsm {aAssign = loc:locs, aOp = op, aArgs = [snd]}]

-- genForIns aasm@(AAsm {aAssign = (loc:locs), aArgs = [fst@(AImm fst'),snd@(ALoc snd')]}) =
--   if (loc == snd')
--     then [aasm]
--     else genForIns'
--
-- -- Ensure that in genForIns' that loc is not in {fst,snd}
-- genForIns' (AAsm {aAssign = locs@(loc:_), aOp = op, aArgs = [fst, snd]}) =
--   [AAsm {aAssign = locs, aOp = Nop, aArgs = [fst]},
--    AAsm {aAssign = locs, aOp = op, aArgs = [ALoc (loc), snd]}]
