module Compile.Asm.FnAsm where 

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Asm.AsmTypes
import Compile.Asm.ExprAsm

import Control.Monad.State

genAsmFnDecl :: IRFuncDef -> String 
genAsmFnDecl fdef@(IRFuncDef name args argTypes retType body argSizes) = 
  let
    (_, (fStr, _, _)) = runState (processBody fdef) ("", 0, Map.empty)
  in
    "function " ++ name ++ 
      " (" ++ (commaSep args) ++ ")" ++ " {" ++ "\n" ++ 
      (argumentCoercion args) ++ "\n" ++ 
      fStr ++ "\n" ++ 
    "}"

commaSep :: [String] -> String
commaSep args = 
  concatMap (\x -> x) $ 
  mapInd (\x -> 
          \i -> (if (i == (length args) - 1) 
                   then x
                   else x ++ ", ")) args

argumentCoercion :: [String] -> String
argumentCoercion args = 
  concatMap (\x -> x ++ " = " ++ x ++ " | 0;" ++ "\n") args

processBody :: IRFuncDef -> AsmState ()
processBody (IRFuncDef {funcBody = body}) = do
  processAST body
  return ()

processAST :: IRAST -> AsmState ()
processAST (IRAST stmt) = do
  processStmt stmt

processStmt :: IRStmt -> AsmState ()
processStmt (IRBlock stmts) = do 
  (s,i,m) <- get 
  let s' = s ++ "\n" ++ "{"
  put(s', i, m)
  processStmts stmts 
  (s'',i', m') <- get
  let s''' = s'' ++ "\n" ++ "}"
  put(s''', i', m')

processStmt (IRNop) = do
  return ()

processStmt (IRDecl dName dTyp dScp) = do
  (s,i,m) <- get
  let m' = Map.insert dName i m
  let i' = i + 1
  put (s, i', m')
  processStmt dScp

processStmt (IRAsgn lval o rval) = do
  lStr <- processIRExpr lval
  rStr <- processIRExpr rval
  (s,i,m) <- get
  let s' = s ++ lStr ++ " = " ++ rStr ++ ";" ++ "\n"
  put (s',i,m)
  -- TODO
  return ()  

processStmt (IRCtrl ctrl) = do
  processCtrl ctrl

processCtrl :: IRCtrl -> AsmState ()
processCtrl (Return (Just rval) _) = do
  rStr <- processIRExpr rval
  (s,i,m) <- get
  let s' = s ++ "return (" ++ rStr ++ " | 0);" ++ "\n"
  put(s',i,m)

processCtrl (Return Nothing _) = do
  (s,i,m) <- get
  let s' = s ++ "return;" ++ "\n"
  put(s',i,m)

processCtrl (If e s1 s2 _) = do
  (s,i,m) <- get
  eStr <- processIRExpr e
  let s' = s ++ "if (" ++ eStr ++ ")" ++ "{" ++ "\n"
  put (s',i,m)
  processStmt s1
  (s'',i',m') <- get
  let s''' = s'' ++ "}" ++ "\n" ++ "else {" ++ "\n"
  put (s''',i',m') 
  processStmt s2
  (s'''',i'',m'') <- get
  let ls = s'''' ++ "}" ++ "\n"
  put (ls,i'',m'')

processCtrl c = do 
  return ()
--  
--processCtrl (While e s1 _) = do
--
--processCtrl (Assert e _) = do
--
--


processStmts :: [IRStmt] -> AsmState ()
processStmts [] = do
  return ()
processStmts (x:xs) = do
  processStmt x
  processStmts xs
  return ()

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]
