module Compile.Frontend.ConstantFold where 

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

type DeclMap = Map.Map String Integer
type UseMap = Map.Map String Int 

-- two passes, 
--  one to determine which decls can be removed
--  one to remove their asgns

constantFold :: FnList -> FnList
constantFold (FnList l p) = FnList (map cfDefn l) p

cfDefn (GFDefn (fdefn@(FDefn {fnName = name,
                              fnBody = body})) pos) = ret 
  where 
    um = genMapAST body
    ret = GFDefn (fdefn {fnBody = cfAST um body}) pos

cfDefn gfdefn = gfdefn

cfAST um (AST (stmt@(Block stmts)) pos) = 
  AST (cfStmt Map.empty um stmt ) pos

cfStmt :: DeclMap -> UseMap -> Stmt -> Stmt
cfStmt dm um (Block stmts) = Block (cfBlock dm um stmts) 
cfStmt dm um (a@(Asgn _ _ _ _ _)) = a
cfStmt dm um (a@(Decl s _ _ scp)) = a {declScope = cfStmt dm um scp}
--  if ((um Map.! s) == 1) 
--    then cfStmt dm um scp
--    else a {declScope = cfStmt dm um scp}
cfStmt dm um (a@(Ctrl c)) = Ctrl $ cfCtrl dm um c
cfStmt dm um (a@(Expr e)) = Expr $ cfExpr dm um e

cfCtrl dm um (If e s1 s2 p) = 
  If (cfExpr dm um e) (cfStmt dm um s1) (cfStmt dm um s2) p
cfCtrl dm um (While e s1 p) = 
  While (cfExpr dm um e) (cfStmt dm um s1) p
cfCtrl dm um (Assert e p) = Assert (cfExpr dm um e) p
cfCtrl dm um (Return (Just e) p) = Return (Just (cfExpr dm um e)) p
cfCtrl dm um c = c

cfExpr dm um e@(Ident s p) = 
  if (Map.member s dm) 
    then ExpInt (dm Map.! s) p Dec 
    else e
cfExpr dm um e@(ExpBinOp o e1 e2 p) = ExpBinOp o (cfExpr dm um e1) (cfExpr dm um e2) p
cfExpr dm um e@(ExpRelOp o e1 e2 p) = ExpRelOp o (cfExpr dm um e1) (cfExpr dm um e2) p
cfExpr dm um e@(ExpPolyEq o e1 e2 p) = ExpPolyEq o (cfExpr dm um e1) (cfExpr dm um e2) p
cfExpr dm um e@(ExpLogOp o e1 e2 p) = ExpLogOp o (cfExpr dm um e1) (cfExpr dm um e2) p
cfExpr dm um e@(ExpUnOp o e1 p) = ExpUnOp o (cfExpr dm um e1) p
cfExpr dm um e@(ExpTernary e1 e2 e3 p) = ExpTernary (cfExpr dm um e1) (cfExpr dm um e2) (cfExpr dm um e3) p
cfExpr dm um e@(ExpFnCall fnName expList p) = ExpFnCall fnName (map (cfExpr dm um) expList) p
cfExpr dm um e = e

cfBlock :: DeclMap -> UseMap -> [Stmt] -> [Stmt]
cfBlock dm um [] = []
cfBlock dm um [x] = [cfStmt dm um x]
cfBlock dm um ((a@(Asgn s o e _ p)):xs) =
  if (((um Map.! s) == 1) && (isInt e)) 
    then cfBlock (Map.insert s (asInt e) dm) um xs
    else a:(cfBlock dm um xs)
cfBlock dm um (x:xs) = (cfStmt dm um x):(cfBlock dm um xs)

isInt (ExpInt i p b) = True
isInt _ = False

asInt (ExpInt i p b) = i
asInt _ = error ("damn yo")

-- Produces the used (really asgn) map - the number of times an ident
-- is assigned to
genMapAST :: AST -> UseMap
genMapAST (AST (stmt@(Block _)) pos) = genMapStmt Map.empty stmt

genMapStmt :: UseMap -> Stmt -> UseMap
genMapStmt um (Asgn s _ _ _ _) = (Map.insert s (1 + (um Map.! s)) um)
genMapStmt um (Block stmts) = foldl genMapStmt um stmts
genMapStmt um (Decl s _ _ scope) = genMapStmt (Map.insert s 0 um) scope
genMapStmt um (Ctrl c) = genMapCtrl um c 
genMapStmt um (Expr e) = um
genMapStmt um SNop = um

genMapCtrl :: UseMap -> Ctrl -> UseMap
genMapCtrl um (If _ s1 s2 _) = genMapStmt (genMapStmt um s1) s2
genMapCtrl um (While _ s1 _) = genMapStmt um s1
genMapCtrl um _ = um
