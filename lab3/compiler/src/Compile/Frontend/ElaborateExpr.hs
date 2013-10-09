module Compile.Frontend.ElaborateExpr where

import Compile.Types 

import Compile.Util.Assert

elabExpr :: Expr -> Expr
elabExpr e = let
  e' = elabExpr' e
  in foldExpr e'

elabExpr' :: Expr -> Expr 
elabExpr' e@(ExpBinOp o e1 e2 p) = checkExpr e $ ExpBinOp (verifyExprOp o p) 
                                                          (elabExpr e1) 
                                                          (elabExpr e2) p
elabExpr' e@(ExpRelOp o e1 e2 p) = checkExpr e $ ExpRelOp (verifyExprOp o p)
                                                          (elabExpr e1) 
                                                          (elabExpr e2) p
elabExpr' e@(ExpPolyEq o e1 e2 p) = checkExpr e $ ExpPolyEq (verifyExprOp o p) 
                                                            (elabExpr e1) 
                                                            (elabExpr e2) p
elabExpr' e@(ExpLogOp And e1 e2 p) = checkExpr e $ ExpTernary (elabExpr e1) 
                                                              (elabExpr e2) 
                                                              (ExpBool False p) p
elabExpr' e@(ExpLogOp Or e1 e2 p) = checkExpr e $ ExpTernary (elabExpr e1) 
                                                             (ExpBool True p)
                                                             (elabExpr e2) p
elabExpr' e@(ExpUnOp o e1 p) = checkExpr e $ ExpUnOp (verifyExprOp o p) 
                                                     (elabExpr e1) p 
elabExpr' e@(ExpTernary e1 e2 e3 p) = checkExpr e $ ExpTernary (elabExpr e1) 
                                                               (elabExpr e2) 
                                                               (elabExpr e3) p
elabExpr' e = checkExpr e $ e

checkExpr (ExpInt n p Dec) = 
  assert  (n <= (2^31)) (show n ++ " too large at " ++ show p)
checkExpr (ExpInt n p Hex) = 
  assert (n <= (2^32)) (show n ++ " too large at " ++ show p)
checkExpr e = (\x -> x)


verifyExprOp Incr p = error ("Undefined use of increment operator in expression at " ++ show p)
verifyExprOp Decr p = error ("Undefined use of increment operator in expression at " ++ show p)
verifyExprOp o _ = o


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
