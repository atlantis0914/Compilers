module Compile.Backend.Spill where

import Compile.Types
import Compile.Backend.Registers

spill :: [AAsm] -> [AAsm]
spill aasms = concatMap (spillAAsm True) aasms

spillVal :: AVal -> AVal
spillVal arg =
  case arg of
    ALoc (AReg i b) -> if i > max_color_num
                       then ALoc $ AMem (i - max_color_num) b
                       else arg
    ALoc (APtr (AReg i True) Nothing _ _ b) -> if i > max_color_num
                                            then ALoc $ AMem (i - max_color_num) True
                                            else arg
    _ -> arg

spillLoc :: ALoc -> ALoc
spillLoc loc =
  case loc of (AReg i b) -> if i > max_color_num
                              then AMem (i - max_color_num) b
                              else loc
              _ -> loc

spillAAsm :: Bool -> AAsm -> [AAsm]
spillAAsm spillArgs aasm@(AAsm [AReg i b] BitwiseNot [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
  in
    if i > max_color_num
        then [AAsm [ASpill b] Nop [ALoc $ AMem (i - max_color_num) b],
              AAsm [ASpill b] BitwiseNot [arg'],
              AAsm [AMem (i - max_color_num) b] Nop [ALoc $ ASpill b]]
        else [aasm]

spillAAsm spillArgs aasm@(AAsm [AIndex b] op [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
  in
    [AAsm [AIndex b] op [arg']]

spillAAsm spillArgs aasm@(AAsm [ASpill b] op [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
  in
    [AAsm [ASpill b] op [arg']]

spillAAsm spillArgs (AAsm [AReg i b] op [ALoc (APtr (AReg j True) index scale off s)]) =
  let
    aasm' = if j > max_color_num
              then [AAsm [ASpill True] Nop [ALoc $ AMem (j - max_color_num) True],
                    AAsm [AReg i b] op [ALoc $ APtr (ASpill True) index scale off b]]
              else [AAsm [AReg i b] op [ALoc (APtr (AReg j True) index scale off b)]]
  in
    if i > max_color_num
      then if j > max_color_num
             then [AAsm [AUtil True] Nop [ALoc $ AMem (j - max_color_num) True],
                   AAsm [AUtil b] Nop [ALoc $ APtr (AUtil True) index scale off b],
                   AAsm [ASpill b] Nop [ALoc $ AMem (i - max_color_num) b],
                   AAsm [ASpill b] op [ALoc (AUtil b)],
                   AAsm [AMem (i - max_color_num) b] Nop [ALoc $ ASpill b]]
             else [AAsm [ASpill b] Nop [ALoc $ AMem (i - max_color_num) b],
                   AAsm [ASpill b] op [ALoc $ APtr (AReg j True) index scale off b],
                   AAsm [AMem (i - max_color_num) b] Nop [ALoc $ ASpill b]]
      else aasm'

spillAAsm spillArgs (AAsm [AReg i b] op [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
    aasm' = AAsm {aAssign = [AReg i b], aOp = op, aArgs = [arg']}
  in
    if i > max_color_num
      then [AAsm [ASpill b] Nop [ALoc $ AMem (i - max_color_num) b],
            AAsm [ASpill b] op [arg'],
            AAsm [AMem (i - max_color_num) b] Nop [ALoc $ ASpill b]]
      else [aasm']

spillAAsm spillArgs (AAsm [APtr (AReg i b) index scale off _] op [ALoc (APtr (AReg j True) index' scale' off' _)]) =
  error ("BANANA ")

spillAAsm spillArgs (AAsm [APtr (AReg i True) index scale off b] op [arg]) =
  let
    arg' = if spillArgs then spillVal arg
                        else arg
    aasm' = case arg' of
              ALoc (AMem j _) -> [AAsm [ASpill b] op [arg'],
                                AAsm [APtr (AReg i True) index scale off b] op [ALoc $ ASpill b]]
              _ -> [AAsm [APtr (AReg i True) index scale off b] op [arg']]
  in
    if i > max_color_num
      then [AAsm [ASpill True] Nop [ALoc $ AMem (i - max_color_num) True],
            AAsm [AUtil b] op [arg'],
            AAsm [APtr (ASpill True) index scale off b] op [ALoc $ AUtil b]]
      else aasm'

spillAAsm _ aasm@(ACtrl (AIf (ALoc (AReg i b)) label err)) =
  if i > max_color_num
    then [AAsm [ASpill b] Nop [ALoc $ AMem (i - max_color_num) b],
          ACtrl (AIf (ALoc (ASpill b)) label err)]
    else [aasm]

spillAAsm _ aasm@(ACtrl c) = [aasm]

spillAAsm _ aasm@(AFnCall n loc locs lives) =
  let
    loc' = spillLoc loc
    locs' = map spillLoc locs
  in
    [AFnCall n loc' locs' lives]
spillAAsm _ aasm = error ("SPILLASM " ++ show aasm)
