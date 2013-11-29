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
  (s,i,m) <- get
  if (Map.member id m) 
    then 
      (do 
        let off = m Map.! id
        let ret = " " ++ "H32[g_stackoff + (" ++ (show off) ++ " | 0) >> 2]" ++ " "
        return ret
      )
    else 
      (do
        let ret = " " ++ id ++ " "
        return ret
      )

processIRExpr (IRExpBinOp Mul e1 e2) = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let ret = "imul(" ++ s1 ++ " | 0," ++ s2 ++ " | 0) | 0"
  return ret
  
processIRExpr (IRExpBinOp op e1 e2) = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let oStr = processIROp op 
  let ret = "(" ++ s1 ++ " | 0) " ++ oStr ++ " (" ++ s2 ++ " | 0)"
  return ret

processIRExpr (IRExpPolyEq op e1 e2) = do
  s1 <- processIRExpr e1
  s2 <- processIRExpr e2
  let oStr = processIROp op 
  let ret = "(" ++ s1 ++ " | 0) " ++ oStr ++ " (" ++ s2 ++ " | 0)"
  return ret
  
processIRExpr e = do 
  return ""

processIROp :: Op -> String
processIROp Mul = error ("Should be manually handling mul")
processIROp Add = "+"
processIROp Sub = "-"
processIROp Div = "/"
processIROp Neg = "-"
processIROp Mod = "%"
processIROp Equ = "=="
