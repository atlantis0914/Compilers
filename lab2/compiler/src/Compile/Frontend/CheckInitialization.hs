module Compile.Frontend.CheckInitialization where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad

import qualified Data.Set as Set

import Debug.Trace

import Compile.Types

checkInitialization :: AST -> Bool
checkInitialization (AST (Block stmts) p) = 
  let
    (l1,r1,b) = checkBlock stmts True
  in
    b

assert :: Bool -> String -> a -> a
assert False msg x = error msg
assert _ _ x = x

verifyDecl doErr liveSet x pos = 
  assert (not ((Set.member x liveSet) && doErr))
         ("Error : variable " ++ x ++ " used uninitialized at " ++ show pos)

usedUndeclared :: Expr -> (Set.Set String) -> (a -> a)
usedUndeclared e decl = 
  assert ((Set.size (Set.intersection (decl) (used e))) == (Set.size decl))
         ("Error : Variable used undeclared in expression : " ++ (show e))

-- produces a (definedSet, liveSet, Bool). Takes a declaredSet 
-- (the declared variables in scope) and also performs undeclared checking.
checkStmt :: Stmt -> Bool -> (Set.Set String, Set.Set String, Bool)
checkStmt(Ctrl (Return e pos)) _ = (Set.empty, used e, True)
checkStmt(Block stmts) doErr = checkBlock stmts doErr
checkStmt (Decl i t pos rest) doErr = 
  let
    (_, liveRest, b1) = checkStmt rest doErr
    setI = verifyDecl doErr liveRest i pos $ Set.singleton(i)
  in
    setI `seq` (Set.empty, Set.difference liveRest setI, b1)

checkStmt(Asgn i o e pos) doErr = (Set.singleton i, used e, True)
checkStmt(Expr e) doErr= (Set.empty, used e, True)
checkStmt(Ctrl (If e s1 s2 pos)) doErr = 
  let
    (decs1, lives1, b1) = checkStmt s1 doErr
    (decs2, lives2, b2) = checkStmt s2 doErr
  in
    decs1 `seq` lives1 `seq` decs2 `seq` lives2 `seq` 
    (Set.intersection decs1 decs2, Set.union lives1 lives2, b1 && b2)

checkStmt(Ctrl (While e s1 pos)) doErr = 
  let
    (decs1, lives1, b1) = checkStmt s1 doErr
  in
    decs1 `seq` lives1 `seq` (Set.empty, Set.union lives1 (used e), b1)

    
checkBlock :: [Stmt] -> Bool -> (Set.Set String, Set.Set String, Bool)
checkBlock [] doErr = (Set.empty, Set.empty, True)
checkBlock [stmt] doErr = checkStmt stmt doErr 
checkBlock (stmt:stmts) doErr  = 
  let
    (decStmt, liveStmt, b1) = checkStmt stmt doErr 
    doErr' = not $ isReturn stmt
    (decRest, liveRest, b2) = checkBlock stmts doErr'
  in
    if (not doErr')
      then decRest `seq` liveRest `seq` (decStmt, liveStmt, b1) 
      else decStmt `seq` liveStmt `seq` decRest `seq` liveRest `seq` 
           (Set.union decStmt decRest, 
            Set.union liveStmt (Set.difference liveRest decStmt), b1 && b2)
   
   

isReturn (Ctrl (Return _ _)) = True
isReturn _ = False


used :: Expr -> (Set.Set String)
used (Ident id _) = Set.singleton(id)
used (ExpBinOp _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpRelOp _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpLogOp _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpPolyEq _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpUnOp _ e1 _) = used e1 
used (ExpTernary e1 e2 e3 _) = Set.union (used e1) $
                                    Set.union (used e2) (used e3)
used _ = Set.empty

