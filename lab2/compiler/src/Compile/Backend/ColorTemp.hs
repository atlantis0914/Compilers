module Compile.Backend.ColorTemp where

import qualified Data.Map as Map
import Compile.Types

colorTemps :: [AAsm] -> ColoringMap -> [AAsm]
colorTemps aasms coloring = map (replaceAsm coloring) aasms

replaceAsm :: ColoringMap -> AAsm -> AAsm
replaceAsm coloring aasm@(AAsm {aAssign = assigns, aOp = op, aArgs = args}) =
  let
    assigns' = map (replaceAssigns coloring) assigns
    args' = map (replaceArgs coloring) args
  in
    AAsm {aAssign = assigns', aOp = op, aArgs = args'}

replaceAsm coloring aasm@(ACtrl (AIf aval label)) =
  let
    aval' = replaceArgs coloring aval
  in
    ACtrl (AIf aval' label)

replaceAsm coloring aasm@(ACtrl a) = aasm

replaceAssigns :: ColoringMap -> ALoc -> ALoc
replaceAssigns coloring (ATemp i) =
  let
    Color c = coloring Map.! (ATemp i)
  in
    AReg c
replaceAssigns coloring loc = loc

replaceArgs :: ColoringMap -> AVal -> AVal
replaceArgs coloring (ALoc (ATemp i)) =
  let
    Color c = coloring Map.! (ATemp i)
  in
    ALoc $ AReg c
replaceArgs coloring val = val
