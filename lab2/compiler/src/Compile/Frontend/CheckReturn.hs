module Compile.Frontend.CheckReturn where

import Compile.Types

checkReturnAST :: AST -> Bool
checkReturnAST (AST stmt _) =
  checkReturnStmt stmt


checkReturnStmt :: Stmt -> Bool
checkReturnStmt (Ctrl (Return _ _)) = True
checkReturnStmt (Ctrl (If _ stmt1 stmt2 _)) =
  checkReturnStmt stmt1 && checkReturnStmt stmt2
checkReturnStmt (Ctrl (While _ _ _)) = False
checkReturnStmt (Block stmts) =
  foldl (\acc stmt -> acc || (checkReturnStmt stmt)) False stmts
checkReturnStmt _ = False

