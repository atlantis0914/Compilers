module Compile.Backend.Spill where

import Compile.Types
import Compile.Backend.Registers

spill :: [AAsm] -> [AAsm]
spill aasms = concatMap spillAAsm aasms

spillVal :: AVal -> AVal
spillVal arg =
  case arg of 
    ALoc (AReg i) -> if i > max_color_num 
                       then ALoc $ AMem $ i - max_color_num
                       else arg
    _ -> arg

spillAAsm :: AAsm -> [AAsm]
spillAAsm aasm@(AAsm {aAssign = [AReg i], aOp = BitwiseNot, aArgs = [arg]}) =
  if i > max_color_num
      then [AAsm {aAssign = [AReg spill_reg_num],
                  aOp = Nop,
                  aArgs = [ALoc $ AMem $ i - max_color_num]},
            AAsm {aAssign = [AReg spill_reg_num],
                  aOp = BitwiseNot,
                  aArgs = [arg]},
            AAsm {aAssign = [AMem $ i - max_color_num],
                  aOp = Nop,
                  aArgs = [ALoc $ AReg spill_reg_num]}]
      else [aasm]

spillAAsm aasm@(AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg]}) =
  let
    arg' = spillVal arg
    aasm' = AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg']}
  in
    if i > max_color_num
      then [AAsm {aAssign = [AReg spill_reg_num],
                  aOp = Nop,
                  aArgs = [ALoc $ AMem $ i - max_color_num]},
            AAsm {aAssign = [AReg spill_reg_num],
                  aOp = op,
                  aArgs = [arg']},
            AAsm {aAssign = [AMem $ i - max_color_num],
                  aOp = Nop,
                  aArgs = [ALoc $ AReg spill_reg_num]}]
      else [aasm']

spillAAsm aasm@(ACtrl (AIf (ALoc (AReg i)) label)) =
  if i > max_color_num
    then [AAsm {aAssign = [AReg spill_reg_num],
                aOp = Nop,
                aArgs = [ALoc $ AMem $ i - max_color_num]},
          ACtrl (AIf (ALoc (AReg spill_reg_num)) label)]
    else [aasm]
spillAAsm aasm@(ACtrl c) = [aasm]
