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
  let
    arg' = spillVal arg
  in
    if i > max_color_num
        then [AAsm {aAssign = [ASpill],
                    aOp = Nop,
                    aArgs = [ALoc $ AMem $ i - max_color_num]},
              AAsm {aAssign = [ASpill],
                    aOp = BitwiseNot,
                    aArgs = [arg']},
              AAsm {aAssign = [AMem $ i - max_color_num],
                    aOp = Nop,
                    aArgs = [ALoc $ ASpill]}]
        else [aasm]

spillAAsm aasm@(AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg]}) =
  let
    arg' = spillVal arg
    aasm' = AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg']}
  in
    if i > max_color_num
      then [AAsm {aAssign = [ASpill],
                  aOp = Nop,
                  aArgs = [ALoc $ AMem $ i - max_color_num]},
            AAsm {aAssign = [ASpill],
                  aOp = op,
                  aArgs = [arg']},
            AAsm {aAssign = [AMem $ i - max_color_num],
                  aOp = Nop,
                  aArgs = [ALoc $ ASpill]}]
      else [aasm']

spillAAsm aasm@(ACtrl (AIf (ALoc (AReg i)) label)) =
  if i > max_color_num
    then [AAsm {aAssign = [ASpill],
                aOp = Nop,
                aArgs = [ALoc $ AMem $ i - max_color_num]},
          ACtrl (AIf (ALoc (ASpill)) label)]
    else [aasm]
spillAAsm aasm@(ACtrl c) = [aasm]
spillAAsm aasm = [aasm]
