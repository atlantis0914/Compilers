module Compile.Frontend.RemVoid where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

type RemMap = Map.Map String Bool

remFn :: FnList -> FnList
remFn (FnList gdecls pos) = (FnList mDecls pos)
  where (_,mDecls) = foldl remGDecl (Map.empty, []) gdecls
 
remGDecl :: (RemMap, [GDecl]) -> GDecl -> (RemMap, [GDecl])
remGDecl (rnMap, prev) (d @(GFDecl _ _)) = (rnMap, prev ++ [d])

remGDecl (rnMap, prev)
            d@(GFDefn (fdefn@(FDefn {fnName = name,
                                     fnBody = body})) pos) = 
  (rnMap', prev ++ defn')
  where
    (rnMap', defn') = getNewBody rnMap name body d

remGDecl (rnMap, prev) g = (rnMap, prev ++ [g])
--     rnMap' = Map.insert name newName rnMap 
--     newName = nameIfLibrary False name -- defns are never library
--    newBody = remBody body rnMap'

getNewBody :: RemMap -> String -> AST -> GDecl -> (RemMap, [GDecl])
getNewBody rnMap name (AST (Block ([Ctrl (Return Nothing _)])) pos) decl = (Map.insert name True rnMap, [decl])
getNewBody rnMap name (AST (Block stmts) pos) decl = 
  if (Map.size rnMap == 0) 
    then (rnMap, [decl])
    else (rnMap, [decl']) -- can't get rid of it
  where 
    decl' = removeMissingFns rnMap decl

removeMissingFns rnMap (GFDefn (fdefn@(FDefn {fnName = name, 
                                              fnBody = body})) pos) = 
  GFDefn (fdefn {fnBody = removeMisBody body rnMap}) pos

removeMisBody :: AST -> RemMap -> AST 
removeMisBody (AST (Block stmts) pos) rnMap = AST (Block stmts') pos 
  where
    stmts' = map (remStmt rnMap) stmts

remStmt :: RemMap -> Stmt -> Stmt 
remStmt rnMap (Ctrl c) = Ctrl $ remCtrl rnMap c
remStmt rnMap (Block stmts) = Block stmts' 
  where
    stmts' = map (remStmt rnMap) stmts

remStmt rnMap (e@(Expr (ExpFnCall fnName expList p))) = 
  if (Map.member fnName rnMap) 
    then SNop
    else Trace.trace ("ret e " ++ fnName) $ e
remStmt rnMap (decl@ (Decl {declScope = scope})) = 
  decl {declScope = remStmt rnMap scope}
remStmt _ s = s

-- remCtrl :: RemMap -> PolyCtrl s -> PolyCtrl s
remCtrl rnMap (If e s1 s2 pos) = (If e s1' s2' pos)
  where 
    s1' = remStmt rnMap s1
    s2' = remStmt rnMap s2

remCtrl rnMap (While e s1 pos) = (While e s1' pos)
  where 
    s1' = remStmt rnMap s1

remCtrl rnMap (Return (Just e) pos) = Return (Just (remExpr rnMap e)) pos
remCtrl _ c = c

remExpr :: RemMap -> Expr -> Expr 
remExpr rm e = e
