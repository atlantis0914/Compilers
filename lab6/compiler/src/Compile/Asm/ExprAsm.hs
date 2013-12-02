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
  let ret = " " ++ id ++ " "
  return ret  

processIRExpr (IRExpNull) = do
  return (show 0)

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
processIROp Add = "+"
processIROp Sub = "-"
processIROp Neg = "-"
processIROp Equ = "=="
