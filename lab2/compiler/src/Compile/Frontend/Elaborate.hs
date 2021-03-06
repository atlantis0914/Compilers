module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map

import qualified Debug.Trace as Trace

assert :: Bool -> String -> a -> a
assert False msg x = error msg
assert _ _ x = x

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
  where expandPStatement s@(PAsgn id Nothing e p) = [PAsgn id Nothing (elabExpr e) p]
        expandPStatement s@(PAsgn id (Just op) e p) = [PAsgn id Nothing (ExpBinOp op (Ident id p) (elabExpr e) p) p]
        expandPStatement s@(PCtrl c) = [PCtrl $ expandCtrl c]
        expandPStatement s@(PBlock stmts) = let
          innerExpanded = expandPStatements stmts
          in 
            case innerExpanded of
              [stmt@(PBlock _)] -> [stmt]
              [stmt] -> [PBlock innerExpanded] -- OSMIUMMMM
              [] -> [] 
              x -> [PBlock innerExpanded]
        expandPStatement s@(PDecl id t p (Just asgn)) = [PDecl id t p Nothing] ++ (expandPStatement asgn)
        expandPStatement s@(PDecl id t p Nothing) = [s]
        expandPStatement s@(PExpr e) = [PExpr (elabExpr e)]

        collapseStmts stmts 
          | (length stmts == 1) = head stmts
          | otherwise = PBlock stmts

        expandCtrl (If e s1 s2 p) = If (elabExpr e) (collapseStmts $ expandPStatement s1) 
                                       (collapseStmts $ expandPStatement s2) p
        expandCtrl (While e s1 p) = While (elabExpr e) (collapseStmts $ expandPStatement s1) p
        expandCtrl (Return e p) = Return (elabExpr e) p


elabParseBlock :: [ParseStmt] -> Stmt
elabParseBlock stmts = elabParseBlock' (Block []) stmts

elabParseStmt :: ParseStmt -> Stmt
elabParseStmt (PAsgn s a e p) = Asgn s a e p
elabParseStmt (PDecl s t p Nothing) = Decl { declName = s, declTyp = t, declPos = p, declScope = SNop}
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

elabExpr :: Expr -> Expr
elabExpr e = let
  e' = elabExpr' e
  in foldExpr e'
elabExpr' :: Expr -> Expr 
elabExpr' e@(ExpBinOp o e1 e2 p) = checkExpr e $ ExpBinOp (verifyExprOp o p) (elabExpr e1) (elabExpr e2) p
elabExpr' e@(ExpRelOp o e1 e2 p) = checkExpr e $ ExpRelOp (verifyExprOp o p) (elabExpr e1) (elabExpr e2) p
elabExpr' e@(ExpPolyEq o e1 e2 p) = checkExpr e $ ExpPolyEq (verifyExprOp o p) (elabExpr e1) (elabExpr e2) p
elabExpr' e@(ExpLogOp And e1 e2 p) = checkExpr e $ ExpTernary (elabExpr e1) 
                                             (elabExpr e2) 
                                             (ExpBool False p) p
elabExpr' e@(ExpLogOp Or e1 e2 p) = checkExpr e $ ExpTernary (elabExpr e1) 
                                            (ExpBool True p)
                                            (elabExpr e2) p
elabExpr' e@(ExpUnOp o e1 p) = checkExpr e $ ExpUnOp (verifyExprOp o p) (elabExpr e1) p 
elabExpr' e@(ExpTernary e1 e2 e3 p) = checkExpr e $ ExpTernary (elabExpr e1) 
                                              (elabExpr e2) 
                                              (elabExpr e3) p
elabExpr' e = checkExpr e $ e

foldExpr :: Expr -> Expr
foldExpr e@(ExpBinOp Mul (ExpInt i1 _ b1) (ExpInt i2 _ _) p) = (ExpInt (i1 * i2) p b1)
foldExpr e@(ExpBinOp Add (ExpInt i1 _ b1) (ExpInt i2 _ _) p) = (ExpInt (i1 + i2) p b1)
foldExpr e@(ExpBinOp Sub (ExpInt i1 _ b1) (ExpInt i2 _ _) p) = (ExpInt (i1 - i2) p b1)
foldExpr e@(ExpBinOp op e1 e2 p) = (ExpBinOp op (foldExpr e1) (foldExpr e2) p)
foldExpr e@(ExpRelOp op e1 e2 p) = (ExpRelOp op (foldExpr e1) (foldExpr e2) p)
foldExpr e@(ExpPolyEq op e1 e2 p) = (ExpPolyEq op (foldExpr e1) (foldExpr e2) p)
foldExpr e@(ExpLogOp op e1 e2 p) = (ExpLogOp op (foldExpr e1) (foldExpr e2) p)
foldExpr e@(ExpTernary e1 e2 e3 p) = (ExpTernary (foldExpr e1) (foldExpr e2) (foldExpr e3) p)
foldExpr e@(ExpUnOp op e1 p) = (ExpUnOp op (foldExpr e1) p)
foldExpr e = e


checkExpr (ExpInt n p Dec) = 
  assert  (n <= (2^31)) (show n ++ " too large at " ++ show p)

checkExpr (ExpInt n p Hex) = 
  assert (n <= (2^32)) (show n ++ " too large at " ++ show p)

checkExpr e = (\x -> x)



verifyExprOp Incr p = error ("Undefined use of increment operator in expression at " ++ show p)
verifyExprOp Decr p = error ("Undefined use of increment operator in expression at " ++ show p)
verifyExprOp o _ = o
