module Compile.Backend.ColorTemp where

import qualified Data.Map as Map
import Compile.Types
import Compile.Backend.Spill

colorTemps :: [AAsm] -> ColoringMap -> [AAsm]
colorTemps aasms coloring = concatMap (replaceAsm coloring) aasms

replaceAsm :: ColoringMap -> AAsm -> [AAsm]
replaceAsm coloring aasm@(AAsm {aAssign = assigns@[AReg i], aOp = op, aArgs = args}) =
  let
    assigns' = map (replaceAssigns coloring) assigns
    args' = map (replaceArgs coloring) args
    args'' = map spillVal args'
  in
    [AAsm {aAssign = assigns', aOp = op, aArgs = args''}]

replaceAsm coloring aasm@(AAsm {aAssign = assigns, aOp = op, aArgs = args@[ALoc (AReg i)]}) =
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

replaceAsm coloring aasm@(ACtrl (AIf aval label)) =
  let
    aval' = replaceArgs coloring aval
  in
    spillAAsm True $ ACtrl (AIf aval' label)

replaceAsm coloring aasm@(ACtrl a) = spillAAsm True aasm

replaceAsm coloring aasm@(AFnCall fnName loc locs lives) =
  let
    loc' = replaceAssigns coloring loc
    locs' = map (replaceAssigns coloring) locs
  in
    spillAAsm True $ AFnCall fnName loc' locs' lives
replaceAsm coloring aasm = error (" replaceAsm EXHAUSTED" ++ show aasm)

replaceAssigns :: ColoringMap -> ALoc -> ALoc
replaceAssigns coloring (APtr base index scale) =
  let
    index' = case index of Nothing -> Nothing
                           Just loc -> Just (replaceAssigns coloring loc)
  in
    APtr (replaceAssigns coloring base) index' scale
replaceAssigns coloring (ATemp i) =
  let
    Color c = coloring Map.! (ATemp i)
  in
    AReg c
replaceAssigns coloring loc = loc

replaceArgs :: ColoringMap -> AVal -> AVal
replaceArgs coloring (ALoc loc) = ALoc $ replaceAssigns coloring loc
replaceArgs coloring aval = aval
