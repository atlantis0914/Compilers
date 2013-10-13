module Compile.Frontend.CheckInitialization where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad

import qualified Data.Set as Set

import Debug.Trace

import Compile.Types

checkInitializationFnList :: FnList -> Bool 
checkInitializationFnList (FnList gdecls pos) =
  all checkInitializationGDecl gdecls 

checkInitializationGDecl :: GDecl -> Bool 
checkInitializationGDecl (GFDecl _ _) = True 
checkInitializationGDecl (GTypeDef _ _ _) = True
checkInitializationGDecl (GFDefn (FDefn {fnArgs = args,
                                         fnArgTypes = argTypes,
                                         fnBody = body}) pos) = 
  let
    body' = prependAsDecls args argTypes body
  in
    body' `seq` checkInitialization args body' 

prependAsDecls :: [String] -> [IdentType] -> AST -> AST
prependAsDecls args argTypes (AST (Block stmts) p) = 
  let
    decls = map (\(a,at) -> Decl {declName = a,
                                  declTyp = at,
                                  declPos = p,
                                  declScope = SNop}) $ zip args argTypes
    decls' = foldr (\decl -> \inner -> decl {declScope = inner}) (Block stmts) decls
    ast' = (AST (Block [decls']) p)
    Decl {declScope = inner} =  decls'
  in
    ast'

checkInitialization :: [String] -> AST -> Bool
checkInitialization args (AST (Block stmts) p) = 
  let
    (l1,r1,b) = checkBlock args stmts True (Set.empty)
  in
    l1 `seq` r1 `seq` b `seq` True

assert :: Bool -> String -> a -> a
assert False msg x = error msg
assert _ _ x = x

isInitDecl args doErr liveSet x pos = 
  assert (not ((Set.member x liveSet) && doErr && (not (x `elem` args))))
         ("Error : variable " ++ x ++ " used uninitialized at " ++ show pos)

-- isDeclaredExpr :: Expr -> (Set.Set String) -> SourcePos -> (a -> a)
isDeclaredExpr e decl pos = 
  assert ((Set.size (Set.intersection (decl) (used e))) == (Set.size $ used e))
         ("Error : Variable used undeclared in expression : " ++ (show e) ++ " at " ++ show pos ++ " setSize = " ++ 
          (show $ Set.size (used e)) ++ " other size = " ++ (show $ Set.size (Set.intersection (decl) (used e))) ++ " decl is " ++ (show decl))

isDeclaredExpr' :: Expr -> (Set.Set String) -> (a -> a)
isDeclaredExpr' e decl = 
  assert ((Set.size (Set.intersection (decl) (used e))) == (Set.size $ used e))
         ("Error : Variable used undeclared in expression : " ++ (show e) ++ " at ")

isDeclaredDecl :: String -> Set.Set String -> (a -> a)
isDeclaredDecl i decls = assert (not (Set.member i decls)) 
                            ("Error : variable " ++ i ++ " declared twice")

isDeclaredAsgn :: String -> Set.Set String -> (a -> a)
isDeclaredAsgn i decls = assert (Set.member i decls) 
                           ("Error : variable " ++ i ++ " assigned to undeclared")

-- produces a (definedSet, liveSet, Bool). Takes a declaredSet 
-- (the declared variables in scope) and also performs undeclared checking.
checkStmt :: [String] -> Stmt -> Bool -> Set.Set String -> (Set.Set String, Set.Set String, Bool)
checkStmt args (Ctrl (Return (Just e) pos)) _ decls = 
  isDeclaredExpr e decls pos (Set.empty, used e, True)
checkStmt args (Ctrl (Return Nothing pos)) _ decls = (Set.empty, Set.empty, True)

checkStmt args (Block stmts) doErr decls = checkBlock args stmts doErr decls
checkStmt args (Decl i t pos rest) doErr decls = 
  let
    (decsRest, liveRest, b1) = isDeclaredDecl i decls $ checkStmt args rest doErr (Set.insert i decls)
    setI = isInitDecl args doErr liveRest i pos $ Set.singleton(i)
  in
    setI `seq` (Set.difference decsRest setI, Set.difference liveRest setI, b1)

checkStmt args (Asgn i o e pos) doErr decls = isDeclaredAsgn i decls (Set.singleton i, used e, False)

checkStmt args (Expr e) doErr decls = isDeclaredExpr' e decls (Set.empty, used e, False)

checkStmt args (Ctrl (If e s1 s2 pos)) doErr decls = 
  let
    (decs1, lives1, b1) = isDeclaredExpr e decls pos $ checkStmt args s1 doErr decls
    (decs2, lives2, b2) = checkStmt args s2 doErr decls
  in
    decs1 `seq` lives1 `seq` decs2 `seq` lives2 `seq` 
    (Set.intersection decs1 decs2, Set.union (used e) (Set.union lives1 lives2), b1 && b2)


checkStmt args (Ctrl (Assert e pos)) doErr decls =  
  isDeclaredExpr e decls pos $ (Set.empty, used e, False)

checkStmt args (Ctrl (While e s1 pos)) doErr decls = 
  let
    (decs1, lives1, b1) = isDeclaredExpr e decls pos $ checkStmt args s1 doErr decls
  in
    decs1 `seq` lives1 `seq` (Set.empty, Set.union lives1 (used e), b1)

checkStmt args SNop _ _ = (Set.empty, Set.empty, False)

    
checkBlock :: [String] -> [Stmt] -> Bool -> Set.Set String -> (Set.Set String, Set.Set String, Bool)
checkBlock _ [] _ _ = (Set.empty, Set.empty, True)
checkBlock args [stmt] doErr decls = checkStmt args stmt doErr decls
checkBlock args (stmt:stmts) doErr decls = 
  let
    (decStmt, liveStmt, b1) = checkStmt args stmt doErr decls
    dropLiveAfter = (isReturn stmt || b1) -- b1 Checks if all sub-branches have returns. 
    (decRest, liveRest, b2) = checkBlock args stmts doErr decls
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

