module Compile.Frontend.Minimize where

import Compile.Types
import qualified Data.Map as Map

import Compile.Frontend.CheckReturn

import qualified Debug.Trace as Trace

minimize :: FnList -> Either String FnList 
minimize (FnList gdecls pos) = 
  let
    minDecls = map dropDeadGDecls gdecls
  in
    Right $ FnList minDecls pos
 
dropDeadGDecls (GFDefn (f@(FDefn {fnBody = body})) pos) = 
  GFDefn (f {fnBody = minimizeAST body}) pos
dropDeadGDecls x = x


minimizeAST :: AST -> AST
minimizeAST (AST (Block stmts) p) = 
  let 
    minimized = dropDeadStmts stmts
  in
    AST (Block minimized) p

dropDeadStmts :: [Stmt] -> [Stmt]
dropDeadStmts [] = []
dropDeadStmts (x:xs) = 
  if (checkReturnStmt x) -- If it returns, everything after is unecessary
    then [dropDeadCode x] -- Drop code that might be dead within x
    else (dropDeadCode x):(dropDeadStmts xs) 

dropDeadCode :: Stmt -> Stmt
dropDeadCode (Decl i t p rest) = Decl i t p (dropDeadCode rest)
dropDeadCode (Ctrl (If e stmt1 stmt2 p)) = Ctrl (If e (dropDeadCode stmt1)
                                                        (dropDeadCode stmt2) p)
dropDeadCode (Ctrl (While e s1 p)) = Ctrl (While e (dropDeadCode s1) p)
dropDeadCode (Block stmts) = Block $ dropDeadStmts stmts
dropDeadCode e = e
