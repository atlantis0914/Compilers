module Compile.Backend.Spill where

import Compile.Types
import Compile.Backend.Registers

spill :: [AAsm] -> [AAsm]
spill aasms = concatMap spillAAsm aasms

spillAAsm :: AAsm -> [AAsm]
spillAAsm aasm@(AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg]}) =
  let
    arg' = case arg of 
             ALoc (AReg i) -> if i > max_color_num 
                                then ALoc $ AMem $ i - max_color_num
                                else arg
             _ -> arg
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
      else [AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg']}]
