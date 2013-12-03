module Compile.Asm.ExprAsm where 

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Asm.AsmTypes

import Control.Monad.State

processIRExpr :: IRExpr -> AsmState String
processIRExpr (IRExpInt i b) = do 
  let ret = " " ++ show i ++ " "
  return ret

processIRExpr (IRExpBool b) = do
  let pVal = if (b) 
               then 1
               else 0
  let ret = " " ++ show pVal ++ " "
  return ret

processIRExpr (IRIdent id i) = do
  let ret = " " ++ (sanitizeDeclVar id) ++ " "
  return ret  

processIRExpr (IRExpNull) = do
  return (show 0)

processIRExpr (IRExpAlloc typ i) = do
  let ret = "(memAlloc(" ++ (show (i `div` 4)) ++ ") | 0)"
  return ret

processIRExpr (IRExpAllocArray typ e i) = do
  eStr <- processIRExpr e
  let ret = "(memAlloc(imul(" ++ eStr ++ " | 0," ++ (show (i `div` 4)) ++ " | 0) | 0) | 0)"
  return ret

processIRExpr (IRExpDereference e typ i) = do
  estr <- processIRExpr e
  let ret = "(pointerDeref(" ++ estr ++ " | 0) | 0)"
  return ret

processIRExpr (IRExpFieldSelect e f typ i1 i2) = do
  estr <- processIRExpr e
  let ret = "(fieldAccess(" ++ estr ++ " | 0," ++ (show (i1 `div` 4)) ++ " | 0) | 0)"
  return ret

processIRExpr (IRExpFnCall fname argList i) = do
  argStrs <- mapM processIRExpr argList 
  let comSep = commaSeparate argStrs
  let ret = "(" ++ fname ++ "(" ++ comSep ++ ") | 0)"
  return ret

processIRExpr (IRExpBinOp Mul e1 e2) = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let ret = "imul(" ++ s1 ++ " | 0," ++ s2 ++ " | 0) | 0"
  return ret

processIRExpr (IRExpBinOp Div e1 e2) = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let ret = "polyDiv(" ++ s1 ++ " | 0," ++ s2 ++ " | 0) | 0"
  return ret

processIRExpr (IRExpBinOp Mod e1 e2) = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let ret = "polyMod(" ++ s1 ++ " | 0," ++ s2 ++ " | 0) | 0"
  return ret
  
processIRExpr (IRExpBinOp op e1 e2) = do
  processBinaryOperation op e1 e2

processIRExpr (IRExpPolyEq op e1 e2) = do
  processBinaryOperation op e1 e2

processIRExpr (IRExpRelOp op e1 e2) = do
  processBinaryOperation op e1 e2

processIRExpr (IRExpLogOp op e1 e2) = do
  processBinaryOperation op e1 e2

processIRExpr (IRExpUnOp op e1) = do
  s1 <- processIRExpr e1
  let oStr = processIROp op
  let ret = "(" ++ oStr ++ "(" ++ s1 ++ " | 0))"
  return ret

processIRExpr (IRExpTernary e1 e2 e3) = do 
  e1str <- processIRExpr e1 
  e2str <- processIRExpr e2 
  e3str <- processIRExpr e3
  let ret = "((" ++ e1str ++ " | 0) ? (" ++ e2str ++ " | 0) : (" ++ e3str ++ " | 0))"
  return ret
  
processIRExpr e = do 
  return ""

processBinaryOperation op e1 e2 = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let oStr = processIROp op 
  let ret = "(" ++ s1 ++ " | 0) " ++ oStr ++ " (" ++ s2 ++ " | 0)"
  return ret

processIROp :: Op -> String
processIROp Mul = error ("Should be manually handling mul")
processIROp Div = error ("Should be manually handling div")
processIROp Add = "+"
processIROp Sub = "-"
processIROp Neg = "-"
processIROp Equ = "=="
processIROp Neq = "!="
processIROp Lt = "<"
processIROp Gt = ">"
processIROp Lte = "<="
processIROp Gte = ">="
processIROp LogicalNot = "!"
processIROp BitwiseNot = "~"
processIROp And = "&&"
processIROp Or = "||"
processIROp BitwiseAnd = "&"
processIROp BitwiseOr = "|"
processIROp BitwiseXOr = "^"
processIROp RShift = ">>"
processIROp LShift = "<<"
processIROp Nop = error ("Shouldn't have Nop in processIROp")

processIROp s = error ("Got op in processIROp: " ++ show s)

commaSeparate [] = ""
commaSeparate [x] = x
commaSeparate (x:xs) = x ++ "," ++ (commaSeparate xs)

sanitizeDeclVar :: String -> String
sanitizeDeclVar "null" = "reserved_null_reserved" 
sanitizeDeclVar "do" = "reserved_do_reserved" 
sanitizeDeclVar "in" = "reserved_in_reserved" 
sanitizeDeclVar "new" = "reserved_new_reserved" 
sanitizeDeclVar "var" = "reserved_var_reserved" 
sanitizeDeclVar "try" = "reserved_try_reserved" 
sanitizeDeclVar x = x
