module Compile.Backend.GenAsm where

import Compile.Types
import qualified Data.Map as Map

regMap = Map.fromList [(0, "%eax"),
                       (1, "%ebx"),
                       (2, "%ecx"),
                       (3, "%edx"),
                       (4, "%esi"),
                       (5, "%edi"),
                       (6, "%ebp"),
                       (7, "%esp"),
                       (8, "%r8d"),
                       (9, "%r9d"),
                       (10, "%r10d"),
                       (11, "%r11d"),
                       (12, "%r12d"),
                       (13, "%r13d"),
                       (14, "%r14d"),
                       (15, "%r15d")]

genAsm :: [AAsm] -> [String]
genAsm aasms =
  let
    prelude = [".text\n", ".globl __c0_main\n", "__c0_main:\n"]
    epilogue = ["ret\n"]
  in
    prelude ++ (map aasmToString aasms) ++ epilogue

aasmToString :: AAsm -> String
aasmToString AAsm {aAssign = [loc], aOp = op, aArgs = [arg]} =
  (opToString op) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"

avalToString :: AVal -> String
avalToString aval =
  case aval of ALoc loc -> alocToString loc
               AImm i -> "$" ++ (show i)

alocToString :: ALoc -> String
alocToString (AReg i) = regMap Map.! i

opToString :: Op -> String
opToString op =
  case op of Mul -> "mull"
             Add -> "addl"
             Sub -> "subl"
             Div -> "divl"
             Neg -> "negl"
             Mod -> "modl"
             Nop -> "movl"
