module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map

elaborate :: ParseAST -> Either String AST 
elaborate (ParseAST (PBlock stmts) p) = 
  let
    expanded = expandPStatements stmts
    elaborated = elabParseBlock expanded
  in
    Right $ AST elaborated p
    
--  Right $ AST (Block $ concatMap elaborate' stmts) p

expandPStatements :: [ParseStmt] -> [ParseStmt]
expandPStatements stmts = concatMap expandPStatement stmts
  where expandPStatement s@(PAsgn _ Nothing _ _) = [s]
        expandPStatement s@(PAsgn id (Just op) e p) = [PAsgn id Nothing (ExpBinOp op (Ident id p) e p) p]
        expandPStatement s@(PCtrl c) = [PCtrl $ expandCtrl c]
        expandPStatement s@(PBlock stmts) = [PBlock (expandPStatements stmts)]
        expandPStatement s@(PDecl id t p (Just asgn)) = [PDecl id t p Nothing, asgn]
        expandPStatement s@(PDecl id t p Nothing) = [s]
        expandPStatement s@(PExpr e) = [s]

        collapseStmts stmts 
          | (length stmts == 1) = head stmts
          | otherwise = PBlock stmts

        expandCtrl (If e s1 s2 p) = If e (collapseStmts $ expandPStatement s1) 
                                         (collapseStmts $ expandPStatement s2) p
        expandCtrl (While e s1 p) = While e (collapseStmts $ expandPStatement s1) p
        expandCtrl (Return e p) = Return e p


elabParseBlock :: [ParseStmt] -> Stmt
elabParseBlock stmts = elabParseBlock' (Block []) stmts

elabParseStmt :: ParseStmt -> Stmt
elabParseStmt (PAsgn s a e p) = Asgn s a e p
elabParseStmt (PDecl s t p Nothing) = error "shouldnt get here"
elabParseStmt (PDecl s t p (Just _)) = error "shouldnt get here just"
            
elabParseStmt (PCtrl c) = Ctrl (elabParseCtrl c)
elabParseStmt (PExpr e) = Expr e
elabParseStmt (PBlock stmts) = elabParseBlock' (Block []) stmts

elabParseCtrl :: ParseCtrl -> Ctrl
elabParseCtrl (If e ps1 ps2 pos) = (If e (elabParseStmt ps1) (elabParseStmt ps2) pos)
elabParseCtrl (While e ps1 pos) = (While e (elabParseStmt ps1) pos)
elabParseCtrl (Return e pos) = (Return e pos)

elabParseBlock' :: Stmt -> [ParseStmt] -> Stmt 
elabParseBlock' curblock [] = curblock
elabParseBlock' (Block curStmts) ((PDecl s t pos Nothing):xs) = 
  Block $ curStmts ++ [Decl {declName = s,
                             declTyp = t,
                             declPos = pos,
                             declScope = elabParseBlock' (Block []) xs}
                      ]
elabParseBlock' (Block curStmts) (x:xs) = elabParseBlock' (Block (curStmts ++ [elabParseStmt x])) xs

  

--elaborate' s@(Decl {extraAsgn = Just(asgn)}) =
--  [s{extraAsgn = Nothing},
--   asgn]
elaborate' :: Stmt -> [Stmt]
elaborate' s@(Asgn _ Nothing _ _) = [s]
elaborate' s@(Asgn id (Just op) e p) = [Asgn id Nothing (ExpBinOp op (Ident id p) e p) p]
elaborate' s = [s]
