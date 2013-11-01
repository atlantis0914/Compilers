module Compile.Backend.ColorTemp where

import qualified Data.Map as Map
import Compile.Types
import Compile.Backend.Spill

colorTemps :: [AAsm] -> ColoringMap -> [AAsm]
colorTemps aasms coloring = concatMap (replaceAsm coloring) aasms

replaceAsm :: ColoringMap -> AAsm -> [AAsm]
replaceAsm coloring aasm@(AAsm assigns@[AReg i b] op [ALoc (APtr base index scale off size)]) =
  let
    assigns' = map (replaceAssigns coloring) assigns
    args' = map (replaceArgs coloring) [ALoc (APtr base index scale off size)]
  in
    spillAAsm True $ AAsm {aAssign = assigns', aOp = op, aArgs = args'}

replaceAsm coloring aasm@(AAsm {aAssign = assigns@[AReg i b], aOp = op, aArgs = args}) =
  let
    assigns' = map (replaceAssigns coloring) assigns
    args' = map (replaceArgs coloring) args
    args'' = map spillVal args'
  in
    [AAsm {aAssign = assigns', aOp = op, aArgs = args''}]

replaceAsm coloring aasm@(AAsm {aAssign = assigns, aOp = op, aArgs = args@[ALoc (AReg i b)]}) =
  let
    assigns' = map (replaceAssigns coloring) assigns
    args' = map (replaceArgs coloring) args
  in
    spillAAsm False $ AAsm {aAssign = assigns', aOp = op, aArgs = args'}

replaceAsm coloring aasm@(AAsm {aAssign = assigns, aOp = op, aArgs = args}) =
  let
    assigns' = map (replaceAssigns coloring) assigns
    args' = map (replaceArgs coloring) args
    aasm = AAsm {aAssign = assigns', aOp = op, aArgs = args'}
  in
    spillAAsm True aasm

replaceAsm coloring aasm@(ACtrl (AIf aval label err)) =
  let
    aval' = replaceArgs coloring aval
  in
    spillAAsm True $ ACtrl (AIf aval' label err)

replaceAsm coloring aasm@(ACtrl a) = spillAAsm True aasm

replaceAsm coloring aasm@(AFnCall fnName loc locs lives) =
  let
    loc' = replaceAssigns coloring loc
    locs' = map (replaceAssigns coloring) locs
  in
    spillAAsm True $ AFnCall fnName loc' locs' lives
replaceAsm coloring aasm = error (" replaceAsm EXHAUSTED" ++ show aasm)

replaceAssigns :: ColoringMap -> ALoc -> ALoc
replaceAssigns coloring (APtr base index scale off b) =
  let
    index' = case index of Nothing -> Nothing
                           Just loc -> Just (replaceAssigns coloring loc b)
  in
    APtr (replaceAssigns coloring base) index' scale off
replaceAssigns coloring (ATemp i b) =
  let
    Color c = coloring Map.! (ATemp i b)
  in
    AReg c b
replaceAssigns coloring loc = loc

replaceArgs :: ColoringMap -> AVal -> AVal
replaceArgs coloring (ALoc loc) = ALoc $ replaceAssigns coloring loc
replaceArgs coloring aval = aval
