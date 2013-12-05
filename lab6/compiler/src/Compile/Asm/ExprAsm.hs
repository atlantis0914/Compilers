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

processIRExpr (IRExpArraySubscript arrE offE typ stride) = do
  arrStr <- processIRExpr arrE
  offE <- processIRExpr offE
  let ret = "(fieldAccess(" ++ arrStr ++ " | 0, (imul((" ++ offE ++ " | 0)," ++ (show (stride `div` 4)) ++ " | 0) | 0)) | 0)"
  return ret

processIRExpr (IRExpDereference e typ i) = do
--  estr <- processIRExpr e
  estr <- chainDeref processIRExpr e
  let ret = "(pointerLoad(" ++ estr ++ " | 0) | 0)"
  return ret

processIRExpr ex@(IRExpFieldSelect e f typ i1 i2) = do
--  estr <- processIRExpr e
  estr <- producePtr processIRExpr ex
  let ret = estr
--  let ret = "(fieldAccess(" ++ estr ++ " | 0," ++ (show (i1 `div` 4)) ++ " | 0) | 0)"
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

-- exprMem :: IRExpr -> AsmState String
-- exprMem (IRExpDereference (IRExpDereference inner typ i) typ' i') = do
--   innerStr <_ process
-- exprMem (IRExpDereference inner typ i) = do
--   innerStr <-  inner
--   let ret = "(pointerDeref(" ++ innerStr ++ " | 0) | 0)"
--   return ret

chainDeref :: (IRExpr -> AsmState String) -> IRExpr -> AsmState String
chainDeref f e@(IRExpDereference (IRIdent id i) typ i2) = do
  f e
  
chainDeref f (IRExpDereference inner typ i) = do
  innerStr <- chainDeref f inner
  let ret = "(pointerLoad(" ++ innerStr ++ " | 0) | 0)"
  return ret

chainDeref f e = f e

chainFieldAccess f (IRExpFieldSelect inner field typ i1 i2) = do
  innerStr <- chainFieldAccess f inner
  let ret = "(fieldShift((" ++ innerStr ++ " | 0)," ++ show (i1 `div` 4) ++ " | 0) | 0)"
  return ret

chainFieldAccess f e = f e 

producePtr :: (IRExpr -> AsmState String) -> IRExpr -> AsmState String
producePtr f e@(IRExpFieldSelect (IRIdent _ _) field typ i1 i2) = do
  f e

producePtr f (IRExpFieldSelect inner field typ i1 i2) = do
  innerStr <- chainFieldAccess (producePtr f) inner
  let ret = "(fieldAccess((" ++ innerStr ++ " | 0)," ++ show (i1 `div` 4) ++ " | 0) | 0)"
  return ret

producePtr f (IRExpDereference inner typ i) = do
  innerStr <- chainDeref (producePtr f) inner
  let ret = "(pointerDeref(" ++ innerStr ++ " |0) | 0)"
  return ret

producePtr f (IRIdent id i) = do
  let ret = " " ++ (sanitizeDeclVar id) ++ " "
  return ret

producePtr f (IRExpArraySubscript arrE offE typ stride) = do
  arrStr <- f arrE
  offE <- f offE
  let ret = "(fieldShift(" ++ arrStr ++ " | 0, (imul((" ++ offE ++ " | 0)," ++ (show (stride `div` 4)) ++ " | 0) | 0)))"
  return ret

producePtr f e = error ("got e" ++ show e)

handleLVal (IRIdent id i) = do
  let ret = " " ++ (sanitizeDeclVar id) ++ " "
  return ret

handleLVal (IRExpDereference inner@(IRIdent _ _) typ i) = do
  innerStr <- chainDeref handleLVal inner
  let ret = "(pointerDeref(" ++ innerStr ++ " | 0) | 0)"
  return ret

handleLVal (IRExpDereference inner typ i) = do
  innerStr <- chainDeref handleLVal inner
  let ret = "(pointerLoad(" ++ innerStr ++ " | 0) | 0)"
  return ret

handleLVal (IRExpFieldSelect inner f typ i1 i2) = do
  innerStr <- handleLVal inner 
  let ret = "(fieldShift((" ++ innerStr ++ " | 0)," ++ show (i1 `div` 4) ++ " | 0) | 0)"
  return ret

handleLVal (IRExpArraySubscript arrE offE typ stride) = do
  arrStr <- processIRExpr arrE
  offE <- processIRExpr offE
  let ret = "(fieldShift(" ++ arrStr ++ " | 0, (imul((" ++ offE ++ " | 0)," ++ (show (stride `div` 4)) ++ " | 0) | 0)))"
  return ret

handleLVal l = error ("err: " ++ show l) 

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
sanitizeDeclVar x = "c0_var_" ++ x
