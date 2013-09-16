module Compile.Backend.GenAsm where

import Compile.Types
import qualified Data.Map as Map

regMap = Map.fromList [(0, "%rax"),
                       (1, "%rbx"),
                       (2, "%rcx"),
                       (3, "%rdx"),
                       (4, "%rsi"),
                       (5, "%rdi"),
                       (6, "%rbp"),
                       (7, "%rsp"),
                       (8, "%r8"),
                       (9, "%r9"),
                       (10, "%r10"),
                       (11, "%r11"),
                       (12, "%r12"),
                       (13, "%r13"),
                       (14, "%r14"),
                       (15, "%r15")]

genAsm :: [AAsm] -> [String]
genAsm aasms =
  let
    prelude = [".globl _main\n", "_main:\n"]
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
