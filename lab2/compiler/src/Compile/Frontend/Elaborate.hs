module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map

elaborate :: AST -> Either String AST 
elaborate (AST (Block stmts) p) = 
  Right $ AST (Block $ concatMap elaborate' stmts) p
 
elaborate' :: Stmt -> [Stmt]
elaborate' s@(Decl {extraAsgn = Just(asgn)}) =
  [s{extraAsgn = Nothing},
   asgn]
elaborate' s@(Asgn _ Nothing _ _) = [s]
elaborate' s@(Asgn id (Just op) e p) = [Asgn id Nothing (ExpBinOp op (Ident id p) e p) p]
elaborate' s = [s]
