module Compile.Frontend.CheckReturn where

import Compile.Types

checkReturnFnList :: FnList -> Bool 
checkReturnFnList (FnList gdecls pos) = 
  all checkGDecl gdecls 

checkGDecl :: GDecl -> Bool 
checkGDecl (GFDecl _ _) = True
checkGDecl (GTypeDef _ _ _) = True
checkGDecl (GFDefn (FDefn {fnReturnType = retType,
                           fnBody = body}) pos) = 
  if (retType == IVoid) 
    then True
    else checkReturnAST body

checkReturnAST :: AST -> Bool
checkReturnAST (AST stmt _) =
  checkReturnStmt stmt


checkReturnStmt :: Stmt -> Bool
checkReturnStmt (Decl i t p rest) = checkReturnStmt rest
checkReturnStmt (Ctrl (Return _ _)) = True
checkReturnStmt (Ctrl (If _ stmt1 stmt2 _)) =
  checkReturnStmt stmt1 && checkReturnStmt stmt2
checkReturnStmt (Ctrl (While _ _ _)) = False
checkReturnStmt (Block stmts) =
  foldl (\acc stmt -> acc || (checkReturnStmt stmt)) False stmts
checkReturnStmt _ = False
