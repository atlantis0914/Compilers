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
aasmToString AAsm {aAssign = [loc], aOp = op, aArgs = [arg]} =
  "  " ++ (opToString op) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"

avalToString :: AVal -> String
avalToString aval =
  case aval of ALoc loc -> alocToString loc
               AImm i -> "$" ++ (show i)

alocToString :: ALoc -> String
alocToString (AReg i) = regMap Map.! i

opToString :: Op -> String
opToString op =
  case op of Mul -> "imull"
             Add -> "addl"
             Sub -> "subl"
             Div -> "idivl"
             Neg -> "negl"
             Mod -> "idivl"
             Nop -> "movl"
