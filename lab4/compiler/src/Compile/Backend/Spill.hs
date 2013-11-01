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
    ALoc (APtr (AReg i) Nothing _ _) -> if i > max_color_num
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

spillAAsm spillArgs aasm@(AAsm {aAssign = [AIndex], aOp = op, aArgs = [arg]}) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
  in
    [AAsm {aAssign = [AIndex], aOp = op, aArgs = [arg']}]

spillAAsm spillArgs aasm@(AAsm {aAssign = [ASpill], aOp = op, aArgs = [arg]}) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
  in
    [AAsm {aAssign = [ASpill], aOp = op, aArgs = [arg']}]

spillAAsm spillArgs (AAsm [AReg i] op [ALoc (APtr (AReg j) index scale off)]) =
  let
    aasm' = if j > max_color_num
              then [AAsm [ASpill] Nop [ALoc $ AMem $ j - max_color_num],
                    AAsm [AReg i] op [ALoc $ APtr ASpill index scale off]]
              else [AAsm [AReg i] op [ALoc (APtr (AReg j) index scale off)]]
  in
    if i > max_color_num
      then if j > max_color_num
             then [AAsm [AUtil] Nop [ALoc $ AMem $ j - max_color_num],
                   AAsm [AUtil] Nop [ALoc $ APtr AUtil index scale off],
                   AAsm [ASpill] Nop [ALoc $ AMem $ i - max_color_num],
                   AAsm [ASpill] op [ALoc AUtil],
                   AAsm [AMem $ i - max_color_num] Nop [ALoc ASpill]]
             else [AAsm [ASpill] Nop [ALoc $ AMem $ i - max_color_num],
                   AAsm [ASpill] op [ALoc $ APtr (AReg j) index scale off],
                   AAsm [AMem $ i - max_color_num] Nop [ALoc ASpill]]
      else aasm'

spillAAsm spillArgs (AAsm [AReg i] op [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
    aasm' = AAsm {aAssign = [AReg i], aOp = op, aArgs = [arg']}
  in
    if i > max_color_num
      then [AAsm [ASpill] Nop [ALoc $ AMem $ i - max_color_num],
            AAsm [ASpill] op [arg'],
            AAsm [AMem $ i - max_color_num] Nop [ALoc ASpill]]
      else [aasm']

spillAAsm spillArgs (AAsm [APtr (AReg i) index scale off] op [ALoc (APtr (AReg j) index' scale' off')]) =
  error ("BANANA ")

spillAAsm spillArgs (AAsm [APtr (AReg i) index scale off] op [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
    aasm' = case arg' of
              ALoc (AMem j) -> [AAsm [ASpill] op [arg'],
                    AAsm [APtr (AReg i) index scale off] op [ALoc ASpill]]
              _ -> [AAsm [APtr (AReg i) index scale off] op [arg']]
  in
    if i > max_color_num
      then [AAsm [ASpill] Nop [ALoc $ AMem $ i - max_color_num],
            AAsm [AUtil] op [arg'],
            AAsm [APtr ASpill index scale off] op [ALoc AUtil]]
      else aasm'

spillAAsm _ aasm@(ACtrl (AIf (ALoc (AReg i)) label err)) =
  if i > max_color_num
    then [AAsm {aAssign = [ASpill],
                aOp = Nop,
                aArgs = [ALoc $ AMem $ i - max_color_num]},
          ACtrl (AIf (ALoc (ASpill)) label err)]
    else [aasm]

spillAAsm _ aasm@(ACtrl c) = [aasm]

spillAAsm _ aasm@(AFnCall n loc locs lives) =
  let
    loc' = spillLoc loc
    locs' = map spillLoc locs
  in
    [AFnCall n loc' locs' lives]
spillAAsm _ aasm = error ("SPILLASM " ++ show aasm)
