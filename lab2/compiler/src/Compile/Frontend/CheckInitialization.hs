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
    (l1,r1,b) = checkBlock stmts True (Set.empty)
  in
    l1 `seq` r1 `seq` b `seq` True

assert :: Bool -> String -> a -> a
assert False msg x = error msg
assert _ _ x = x

isInitDecl doErr liveSet x pos = 
  assert (not ((Set.member x liveSet) && doErr))
         ("Error : variable " ++ x ++ " used uninitialized at " ++ show pos)

isDeclaredExpr :: Expr -> (Set.Set String) -> (a -> a)
isDeclaredExpr e decl = 
  assert ((Set.size (Set.intersection (decl) (used e))) == (Set.size $ used e))
         ("Error : Variable used undeclared in expression : " ++ (show e))

isDeclaredDecl :: String -> Set.Set String -> (a -> a)
isDeclaredDecl i decls = assert (not (Set.member i decls)) 
                            ("Error : variable " ++ i ++ " declared twice")

isDeclaredAsgn :: String -> Set.Set String -> (a -> a)
isDeclaredAsgn i decls = assert (Set.member i decls) 
                           ("Error : variable " ++ i ++ " assigned to undeclared")

-- produces a (definedSet, liveSet, Bool). Takes a declaredSet 
-- (the declared variables in scope) and also performs undeclared checking.
checkStmt :: Stmt -> Bool -> Set.Set String -> (Set.Set String, Set.Set String, Bool)
checkStmt(Ctrl (Return e pos)) _ decls = 
  isDeclaredExpr e decls (Set.empty, used e, True)

checkStmt(Block stmts) doErr decls = checkBlock stmts doErr decls
checkStmt (Decl i t pos rest) doErr decls = 
  let
    (decsRest, liveRest, b1) = isDeclaredDecl i decls $ checkStmt rest doErr (Set.insert i decls)
    setI = isInitDecl doErr liveRest i pos $ Set.singleton(i)
  in
    setI `seq` (Set.difference decsRest setI, Set.difference liveRest setI, b1)

checkStmt(Asgn i o e pos) doErr decls = isDeclaredAsgn i decls (Set.singleton i, used e, False)

checkStmt(Expr e) doErr decls = isDeclaredExpr e decls (Set.empty, used e, False)

checkStmt(Ctrl (If e s1 s2 pos)) doErr decls = 
  let
    (decs1, lives1, b1) = isDeclaredExpr e decls $ checkStmt s1 doErr decls
    (decs2, lives2, b2) = checkStmt s2 doErr decls
  in
    decs1 `seq` lives1 `seq` decs2 `seq` lives2 `seq` 
    (Set.intersection decs1 decs2, Set.union (used e) (Set.union lives1 lives2), b1 && b2)

checkStmt(Ctrl (While e s1 pos)) doErr decls = 
  let
    (decs1, lives1, b1) = isDeclaredExpr e decls $ checkStmt s1 doErr decls
  in
    decs1 `seq` lives1 `seq` (Set.empty, Set.union lives1 (used e), b1)

checkStmt SNop _ _ = (Set.empty, Set.empty, False)

    
checkBlock :: [Stmt] -> Bool -> Set.Set String -> (Set.Set String, Set.Set String, Bool)
checkBlock [] _ _ = (Set.empty, Set.empty, True)
checkBlock [stmt] doErr decls = checkStmt stmt doErr decls
checkBlock (stmt:stmts) doErr decls = 
  let
    (decStmt, liveStmt, b1) = checkStmt stmt doErr decls
    dropLiveAfter = (isReturn stmt || b1) -- b1 Checks if all sub-branches have returns. 
    (decRest, liveRest, b2) = checkBlock stmts doErr decls
  in
    if (dropLiveAfter)
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

