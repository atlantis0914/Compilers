module Compile.Frontend.RenameFn where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

type RenameMap = Map.Map String String

nameIfLibrary :: Bool -> String -> String
nameIfLibrary True s = "" ++ s
nameIfLibrary False s = "_c0_" ++ s

renameFn :: FnList -> FnList
renameFn (FnList gdecls pos) = (FnList mDecls pos)
  where (_,mDecls) = foldl renameGDecl (Map.empty, []) gdecls

renameGDecl :: (RenameMap, [GDecl]) -> GDecl -> (RenameMap, [GDecl])
renameGDecl (rnMap, prev)
            (GFDecl (fdecl@(FDecl {gdeclName = name,
                                   gdeclIsLibrary = isLibrary})) pos) =
  (rnMap',
   prev ++ [GFDecl (fdecl {gdeclName = newName}) pos])
  where
    rnMap' = Map.insert name newName rnMap
    newName = nameIfLibrary isLibrary name

renameGDecl (rnMap, prev)
            (GFDefn (fdefn@(FDefn {fnName = name,
                                   fnBody = body})) pos) =
  (rnMap', prev ++ [GFDefn (fdefn {fnName = newName,
                                   fnBody = newBody}) pos])
  where
    rnMap' = Map.insert name newName rnMap
    newName = nameIfLibrary False name -- defns are never library
    newBody = renameBody body rnMap'

renameGDecl (rnMap, prev) g = (rnMap, prev ++ [g])

renameBody :: AST -> RenameMap -> AST
renameBody (AST (Block stmts) pos) rnMap = AST (Block stmts') pos
  where
    stmts' = map (renameStmt rnMap) stmts

renameStmt :: RenameMap -> Stmt -> Stmt
renameStmt rnMap (Asgn (LExpr e1 p) op e2 b pos) = Asgn (LExpr (renameExpr rnMap e1) p) op (renameExpr rnMap e2) b pos
renameStmt rnMap (Ctrl c) = Ctrl $ renameCtrl rnMap c
renameStmt rnMap (Block stmts) = Block stmts'
  where
    stmts' = map (renameStmt rnMap) stmts
renameStmt rnMap (Expr e) = Expr (renameExpr rnMap e)
renameStmt rnMap (decl@ (Decl {declScope = scope})) =
  decl {declScope = renameStmt rnMap scope}
renameStmt _ SNop = SNop
-- renameStmt _ s = s

-- renameCtrl :: RenameMap -> PolyCtrl s -> PolyCtrl s
renameCtrl rnMap (If e s1 s2 pos) = (If e' s1' s2' pos)
  where
    e' = renameExpr rnMap e
    s1' = renameStmt rnMap s1
    s2' = renameStmt rnMap s2

renameCtrl rnMap (While e s1 pos) = (While e' s1' pos)
  where
    e' = renameExpr rnMap e
    s1' = renameStmt rnMap s1

renameCtrl rnMap (Assert e pos) = (Assert (renameExpr rnMap e) pos)

renameCtrl rnMap (Return (Just e) pos) = Return (Just (renameExpr rnMap e)) pos

renameCtrl _ c = c

renameExpr :: RenameMap -> Expr -> Expr

renameExpr rm e@(ExpInt _ _ _) = e
renameExpr rm e@(ExpBool _ _) = e
renameExpr rm e@(Ident _ _) = e
renameExpr rm e@(ExpNull _) = e
renameExpr rm e@(ExpAlloc _ _) = e
renameExpr rm e@(ExpBinOp o e1 e2 p) = ExpBinOp o (renameExpr rm e1) (renameExpr rm e2) p
renameExpr rm e@(ExpRelOp o e1 e2 p) = ExpRelOp o (renameExpr rm e1) (renameExpr rm e2) p
renameExpr rm e@(ExpPolyEq o e1 e2 p) = ExpPolyEq o (renameExpr rm e1) (renameExpr rm e2) p
renameExpr rm e@(ExpLogOp o e1 e2 p) = ExpLogOp o (renameExpr rm e1) (renameExpr rm e2) p
renameExpr rm e@(ExpUnOp o e1 p) = ExpUnOp o (renameExpr rm e1) p
renameExpr rm e@(ExpTernary e1 e2 e3 p) = ExpTernary (renameExpr rm e1) (renameExpr rm e2) (renameExpr rm e3) p
renameExpr rm e@(ExpFnCall fnName expList p) = ExpFnCall (renameFnName rm fnName) (map (renameExpr rm) expList) p
renameExpr rm e@(ExpAllocArray id e1 p) = ExpAllocArray id (renameExpr rm e1) p
renameExpr rm e@(ExpBinMem o e1 e2 p) = ExpBinMem o (renameExpr rm e1) (renameExpr rm e2) p
renameExpr rm e@(ExpUnMem o e1 p) = ExpUnMem o (renameExpr rm e1) p 
renameExpr rm e = error ("Got : " ++ show e)
-- renameExpr rm e = e

-- Special cases for globally declared fns
renameFnName rm ("main") = nameIfLibrary False ("main") -- main is never library
renameFnName rm f = 
  case (Map.lookup f rm) of 
    Just t -> t
    Nothing -> error ("Error couldn't find it") 
-- renameFnName rm f = Trace.trace (" naming : " ++ (rm Map.! f)) $ rm Map.! f
