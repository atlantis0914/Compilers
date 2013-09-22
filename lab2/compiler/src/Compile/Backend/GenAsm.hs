module Compile.Backend.GenAsm where

import Compile.Types
import qualified Data.Map as Map

import Compile.Backend.Registers

genAsm :: [AAsm] -> [String]
genAsm aasms =
  let
    prelude = [".globl __c0_main\n", "__c0_main:\n"]
    epilogue = ["  ret\n"]
  in
    prelude ++ (map aasmToString aasms) ++ epilogue

aasmToString :: AAsm -> String

aasmToString AAsm {aAssign = [loc], aOp = Neg, aArgs = [arg]} =
  (aasmToString (AAsm {aAssign = [loc], aOp = Nop, aArgs = [arg]}) ++ "  " ++ (opToString Neg) ++ " " ++ (alocToString loc) ++ "\n")

aasmToString AAsm {aAssign = [loc], aOp = Div, aArgs = [snd]}  = divModToString loc snd Div
aasmToString AAsm {aAssign = [loc], aOp = Mod, aArgs = [snd]}  = divModToString loc snd Mod

aasmToString AAsm {aAssign = [loc], aOp = op, aArgs = [arg]} =
  "  " ++ (opToString op) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"

avalToString :: AVal -> String
avalToString aval =
  case aval of ALoc loc -> alocToString loc
               AImm i -> "$" ++ (show i)

alocToString :: ALoc -> String
alocToString (AReg i) =
  if i > max_color_num + 1
    then alocToString (AMem $ i - max_color_num)
    else regMap Map.! i
alocToString (AMem i) =  "-" ++ (show (i * 4)) ++ "(%rsp)"

divModToString :: ALoc -> AVal -> Op -> String
divModToString fst snd op = (divPrologue fst snd) ++ (divEpilogue fst op)

divPrologue :: ALoc -> AVal -> String
divPrologue fst snd = 
  "  " ++ "movl" ++ " " ++ (alocToString fst) ++ ", " ++ "%eax" ++ "\n" ++ 
  "  " ++ "cltd" ++ "\n" ++ 
  "  " ++ (opToString Div) ++ " " ++ (avalToString snd) ++ "\n"

divEpilogue :: ALoc -> Op -> String
divEpilogue fst op 
  | op == Div = "  " ++ "movl" ++ " " ++ "%eax" ++ "," ++ (alocToString fst) ++ "\n"
  | op == Mod = "  " ++ "movl" ++ " " ++ "%edx" ++ "," ++ (alocToString fst) ++ "\n"

opToString :: Op -> String
opToString op =
  case op of Mul -> "imull"
             Add -> "addl"
             Sub -> "subl"
             Div -> "idivl"
             Neg -> "negl"
             Mod -> "idivl"
             Nop -> "movl"
