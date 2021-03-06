module Compile.Backend.GenAsm where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Backend.Registers

genAsm :: [AAsm] -> [String]
genAsm aasms =
  let
    prelude = [".globl __c0_main\n", "__c0_main:\n"]
    epilogue = ["  ret\n", "error:\n", "movw $1, %ax\n", "movw $0, %bx\n", "divw %bx\n"]
  in
    prelude ++ (map aasmToString aasms) ++ epilogue

cmpAsm :: ALoc -> AVal -> String
cmpAsm loc val =
  "cmpl " ++ (avalToString val) ++ ", " ++ (alocToString loc)

aasmToString :: AAsm -> String

{-aasmToString aasm | Trace.trace (show aasm) False = undefined-}

aasmToString AAsm {aAssign = [loc], aOp = LogicalNot, aArgs = [arg]} =
  "  movl " ++ (avalToString arg) ++ ", " ++ (alocToString loc) ++ "\n  not " ++ (alocToString loc) ++ "\n  and $1, " ++ (alocToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Lt, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setl " ++ (alocByteToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Lte, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setle " ++ (alocByteToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Gt, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setg " ++ (alocByteToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Gte, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setge " ++ (alocByteToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Equ, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  sete " ++ (alocByteToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Neq, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setne " ++ (alocByteToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = Neg, aArgs = [arg]} =
  (aasmToString (AAsm {aAssign = [loc], aOp = Nop, aArgs = [arg]}) ++ "  " ++ (opToString Neg) ++ " " ++ (alocToString loc) ++ "\n")

aasmToString AAsm {aAssign = [loc], aOp = Div, aArgs = [snd]} = divModToString loc snd Div
aasmToString AAsm {aAssign = [loc], aOp = Mod, aArgs = [snd]} = divModToString loc snd Mod

aasmToString AAsm {aAssign = [loc], aOp = LShift, aArgs = [snd]} =
  (checkLt32 snd) ++ (checkGte0 snd) ++
  "  movb " ++ (avalByteToString snd) ++ ", %cl\n  " ++ (opToString LShift) ++ " %cl, " ++ (alocToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = RShift, aArgs = [snd]} =
  (checkLt32 snd) ++ (checkGte0 snd) ++
  "  movb " ++ (avalByteToString snd) ++ ", %cl\n  " ++ (opToString RShift) ++ " %cl, " ++ (alocToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = BitwiseNot, aArgs = [arg]} =
  "  " ++ (opToString BitwiseNot) ++ " " ++ (alocToString loc) ++ "\n"

aasmToString AAsm {aAssign = [loc], aOp = op, aArgs = [arg]} =
  "  " ++ (opToString op) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"

aasmToString (ACtrl (ALabel i)) =
  "\nlabel" ++ show i ++ ":\n"

aasmToString (ACtrl (AGoto i)) =
  "  jmp label" ++ show i ++ "\n"

aasmToString (ACtrl (AIf aval label)) =
  "  testb " ++ (avalByteToString aval) ++ ", " ++ (avalByteToString aval) ++ "\n  jnz label" ++ (show label) ++ "\n"

aasmToString (ACtrl (ARet _)) =
  "  ret" ++ "\n"

avalByteToString :: AVal -> String
avalByteToString aval =
  case aval of ALoc loc -> (alocByteToString loc)
               _ -> (avalToString aval)

avalToString :: AVal -> String
avalToString aval =
  case aval of ALoc loc -> alocToString loc
               ABool b -> if b then "$1"
                               else "$0"
               AImm i -> "$" ++ (show i)

safeLookup :: Int -> Map.Map Int String -> String -> String
safeLookup i map s =
  case Map.lookup i map of Nothing -> error ("NOT FOUND " ++ (show i) ++ " wtf " ++ s)
                           Just r -> r
alocByteToString :: ALoc -> String
alocByteToString (AReg i) =
  if i > max_color_num + 1
    then alocToString (AMem $ i - max_color_num)
    else safeLookup i regByteMap "FUCK"
alocByteToString (AMem i) =
  alocToString (AMem i)

alocToString :: ALoc -> String
alocToString (AReg i) =
  if i > max_color_num + 1
    then alocToString (AMem $ i - max_color_num)
    else safeLookup i regMap "SHIT"
alocToString (AMem i) =  "-" ++ (show (i * 4)) ++ "(%rsp)"
alocToString (ATemp i) =
  error "There's still an temp!"

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
             RShift -> "sarl"
             LShift -> "sall"
             BitwiseAnd -> "andl"
             BitwiseOr -> "orl"
             BitwiseNot -> "notl"
             BitwiseXOr -> "xorl"
             Incr -> "addl"
             Decr -> "subl"
             _ -> error ("error matching " ++ show op)

checkGte0 :: AVal -> String
checkGte0 aval =
  "  cmpl $0, " ++ (avalToString aval) ++ "\n  jl error\n"

checkLt32 :: AVal -> String
checkLt32 aval =
  "  cmpl $32, " ++ (avalToString aval) ++ "\n  jge error\n"
