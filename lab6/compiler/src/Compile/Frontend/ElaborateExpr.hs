module Compile.Frontend.ElaborateExpr where

import Compile.Types 

import Compile.Util.Assert
import Compile.Util.IdentTypeUtil
import Compile.Frontend.ElaborateType

import qualified Debug.Trace as Trace

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
elabExpr' e@(ExpPolyEq o e1 e2 p) = checkExpr e $ExpPolyEq (verifyExprOp o p) 
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
elabExpr' e@(ExpFnCall fnName expList p) = checkExpr e $ ExpFnCall fnName
                                                       (map elabExpr' expList) p
elabExpr' e@(ExpAllocArray id e1 p) = checkExpr e $ ExpAllocArray id (elabExpr e1) p
-- Elaborates e1->e2 into (*e1).e2
elabExpr' e@(ExpBinMem FDereference e1 e2 p) = ExpBinMem Select (ExpUnMem PDereference (elabExpr e1) p)
                                                                (elabExpr e2) p
elabExpr' e@(ExpBinMem o e1 e2 p) = checkExpr e $ ExpBinMem o (elabExpr e1)
                                                              (elabExpr e2) p
elabExpr' e@(ExpUnMem o e1 p) = checkExpr e $ ExpUnMem o (elabExpr e1) p 
elabExpr' e@(ExpBool _ _) = checkExpr e $ e
elabExpr' e@(ExpInt _ _ _) = checkExpr e $ e
elabExpr' e@(ExpNull _) = checkExpr e $ e
elabExpr' e@(ExpAlloc _ _) = checkExpr e $ e 
elabExpr' e@(Ident _ _) = e
elabExpr' e = error ("fucked up on e : " ++ show e)
-- elabExpr' e = checkExpr e $ e

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
foldExpr e@(ExpUnOp BitwiseNot (ExpUnOp BitwiseNot e' p2) p1) = foldExpr e'
foldExpr e@(ExpUnOp Neg (ExpUnOp Neg e' p2) p1) = foldExpr e'
foldExpr e@(ExpUnOp op e1 p) = (ExpUnOp op (foldExpr e1) p)
foldExpr e@(ExpFnCall n eList p) = (ExpFnCall n (map foldExpr eList) p)
foldExpr e = e

elabExprTD :: TypeDefs -> Expr -> Expr
elabExprTD td e@(ExpAllocArray id e1 p) = ExpAllocArray (elaborateTDIdentType td id) 
                                                        (elabExprTD td e1) p
elabExprTD td e@(ExpAlloc id p) = ExpAlloc (elaborateTDIdentType td id) p
elabExprTD td e@(ExpBinOp o e1 e2 p) = ExpBinOp o (elabExprTD td e1) 
                                                  (elabExprTD td e2) p
elabExprTD td e@(ExpRelOp o e1 e2 p) = ExpRelOp o (elabExprTD td e1) 
                                                  (elabExprTD td e2) p
elabExprTD td e@(ExpPolyEq o e1 e2 p) = ExpPolyEq o (elabExprTD td e1) 
                                                 (elabExprTD td e2) p
elabExprTD td e@(ExpLogOp _ _ _ _) = error ("shouldn't have log ops in elabExprTD")
elabExprTD td e@(ExpUnOp o e1 p) = ExpUnOp o (elabExprTD td e1) p 
elabExprTD td e@(ExpTernary e1 e2 e3 p) = ExpTernary (elabExprTD td e1) 
                                                     (elabExprTD td e2) 
                                                     (elabExprTD td e3) p
elabExprTD td e@(ExpFnCall fnName expList p) = ExpFnCall fnName
                                                       (map (elabExprTD td) expList) p
elabExprTD td e@(ExpBinMem FDereference e1 e2 p) = error ("shouldn't have fDereference in elabExprTD : " ++ show p)
elabExprTD td e@(ExpBinMem Select e1 e2 p) = selectOrder (elabExprTD td e1) (elabExprTD td e2) p
elabExprTD td e@(ExpBinMem o e1 e2 p) = ExpBinMem o (elabExprTD td e1) (elabExprTD td e2) p
elabExprTD td e@(ExpUnMem o e1 p) = checkExpr e $ ExpUnMem o (elabExprTD td e1) p 
elabExprTD _ e = e


selectOrder e1 e2@(Ident _ _) p = ExpBinMem Select e1 e2 p
selectOrder e1 e2 p = 
  case (extractLIdent e2 Nothing) of
    (id, Nothing) -> ExpBinMem Select e1 id p
    (id, Just (o, e2')) -> ExpBinMem o (ExpBinMem Select e1 id p) e2' p
  where

extractLIdent e@(Ident _ _) save = (e,save)
extractLIdent (ExpBinMem o e1 e2 p) save = 
  case save of 
    Nothing -> extractLIdent e1 (Just (o, e2))
    _ -> extractLIdent e1 save
  
