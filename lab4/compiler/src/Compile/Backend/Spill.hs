module Compile.Backend.Spill where

import Compile.Types
import Compile.Backend.Registers

spill :: [AAsm] -> [AAsm]
spill aasms = concatMap (spillAAsm True) aasms

spillVal :: AVal -> AVal
spillVal arg =
  case arg of
    ALoc (AReg i) -> if i > max_color_num
                       then ALoc $ AMem $ i - max_color_num
                       else arg
    _ -> arg

spillLoc :: ALoc -> ALoc
spillLoc loc =
  case loc of (AReg i) -> if i > max_color_num
                            then AMem $ i - max_color_num
                            else loc
              _ -> loc

spillAAsm :: Bool -> AAsm -> [AAsm]
spillAAsm spillArgs aasm@(AAsm {aAssign = [AReg i], aOp = BitwiseNot, aArgs = [arg]}) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
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

spillAAsm spillArgs aasm@(AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg]}) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
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

spillAAsm spillArgs aasm@(AAsm {aAssign = [APtr (AReg i) None off], aOp = op, aArgs = [arg]}) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
    aasm' = AAsm {aAssign = [APtr (AReg i) None off], aOp = op, aArgs = [arg']}
  in
    if i > max_color_num
      then [AAsm {aAssign = [ASpill],
                  aOp = Nop,
                  aArgs = [ALoc $ AMem $ i - max_color_num]},
            AAsm {aAssign = [APtr ASpill None off],
                  aOp = op,
                  aArgs = [arg']}]
      else [aasm']

spillAAsm spillArgs aasm@(AAsm {aAssign = [APtr (AReg i) (Just (AReg j)) off], aOp = op, aArgs = [arg]}) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
    aasm' = AAsm {aAssign = [APtr (AReg i) (Just $ AReg j) off], aOp = op, aArgs = [arg']}
    base = if i > max_color_num then ASpill
                                else AReg i
    spillBase = if i > max_color_num
      then [AAsm {aAssign = [ASpill],
                  aOp = Nop,
                  aArgs = [ALoc $ AMem $ i - max_color_num]}]
      else []
    index = if j > max_color_num then Just AIndex
                                 else Just $ AReg i
    spillIndex = if j > max_color_num
      then [AAsm {aAssign = [AIndex],
                  aOp = Nop,
                  aArgs = [ALoc $ AMem $ j - max_color_num]}]
      else []
  in
    spillBase ++ spillIndex ++
      [AAsm {aAssign = [APtr base index off],
             aOp = op,
             aArgs = [arg']}]

spillAAsm _ aasm@(ACtrl (AIf (ALoc (AReg i)) label)) =
  if i > max_color_num
    then [AAsm {aAssign = [ASpill],
                aOp = Nop,
                aArgs = [ALoc $ AMem $ i - max_color_num]},
          ACtrl (AIf (ALoc (ASpill)) label)]
    else [aasm]

spillAAsm _ aasm@(ACtrl c) = [aasm]

spillAAsm _ aasm@(AFnCall n loc locs lives) =
  let
    loc' = spillLoc loc
    locs' = map spillLoc locs
  in
    [AFnCall n loc' locs' lives]
