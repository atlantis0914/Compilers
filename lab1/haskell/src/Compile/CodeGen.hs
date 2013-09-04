{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Currently just a pseudolanguage with 3-operand instructions and arbitrarily many temps.
-}
module Compile.CodeGen where

import Compile.Types
import qualified Data.Map as Map

type Alloc = (Map.Map String Int, Int)

codeGen :: AST -> [AAsm]
codeGen (Block decls stmts _) = let
  temps = Map.fromList $ zip (map declName decls) [0..]
  in concatMap (genStmt (temps, (length decls))) stmts

genStmt :: Alloc -> Stmt -> [AAsm]
genStmt alloc (Return e _) = genExp alloc e (AReg 0)
genStmt (a,n) (Asgn v o e s) = let
  l = ATemp $ a Map.! v
  e' = case o of
         Nothing -> e
         Just op -> ExpBinOp op (Ident v s) e s
  in genExp (a,n) e' l

genExp :: Alloc -> Expr -> ALoc -> [AAsm]
genExp _ (ExpInt n _) l = [AAsm [l] Nop [AImm $ fromIntegral n]]
genExp (a,_) (Ident s _) l = [AAsm [l] Nop [ALoc $ ATemp $ a Map.! s]]
genExp (a,n) (ExpBinOp op e1 e2 _) l = let
  i1 = genExp (a, n + 1) e1 (ATemp n)
  i2 = genExp (a, n + 2) e2 (ATemp $ n + 1)
  c  = [AAsm [l] op [ALoc $ ATemp n, ALoc $ ATemp $ n + 1]]
  in i1 ++ i2 ++ c
genExp (a,n) (ExpUnOp op e _) l = let
  i1 = genExp (a, n + 1) e (ATemp n)
  c  = [AAsm [l] op [ALoc $ ATemp n]]
  in i1 ++ c
