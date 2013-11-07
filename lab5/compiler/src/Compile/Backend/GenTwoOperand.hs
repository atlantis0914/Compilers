module Compile.Backend.GenTwoOperand where

import Compile.Types
import Compile.Util.Graph
import Compile.Util.AbstractAssembly

import Data.List
import qualified Data.Map as Map

import Debug.Trace as Trace

getLocSize :: ALoc -> Bool
getLocSize (AReg _ b) = b
getLocSize (ATemp _ b) = b
getLocSize (ASpill b) = b
getLocSize (AUtil b) = b
getLocSize (AIndex b) = b
getLocSize (AArg _ b) = b
getLocSize (APtr _ _ _ _ b) = b
getLocSize (AMem _ b) = b

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

conv (ATemp i _) (ALoc loc) = ATemp i (getLocSize loc)
conv (AMem i _) (ALoc loc) = AMem i (getLocSize loc)
conv (AReg i _) (ALoc loc) = AReg i (getLocSize loc)
conv (ASpill _) (ALoc loc) = ASpill (getLocSize loc)
conv (AIndex _) (ALoc loc) = AIndex (getLocSize loc)
conv loc (AImm _) = loc
conv loc1 loc2 = error ("ROFL " ++ show loc1 ++ " wat" ++ show loc2)

genForIns' aasm@(AAsm {aAssign = [loc], aOp = op, aArgs = [fst, snd]}) =
  [AAsm {aAssign = [conv loc fst], aOp = Nop, aArgs = [fst]},
   AAsm {aAssign = [conv loc fst], aOp = op, aArgs = [snd]}]

genForDiv aasm@(AAsm {aAssign = loc:locs, aOp = op, aArgs = [fst, snd]}) =
  [AAsm {aAssign = loc:locs, aOp = Nop, aArgs = [fst]},
   AAsm {aAssign = loc:locs, aOp = op, aArgs = [ALoc loc, snd]}]
