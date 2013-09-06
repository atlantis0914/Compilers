{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Currently just a pseudolanguage with 3-operand instructions and arbitrarily many temps.
-}
module Compile.Backend.CodeGen where

import Compile.Types
import qualified Data.Map as Map

type Alloc = (Map.Map String Int, Int)

-- Generates the AAsm from an AST
codeGen :: AST -> [AAsm]
codeGen (Block decls stmts _) = let
  -- Creates a mapping from var to its index.
  temps = Map.fromList $ zip (map declName decls) [0..]
  in concatMap (genStmt (temps, length decls)) stmts

-- Generates AAsm from a statement
genStmt :: Alloc -> Stmt -> [AAsm]
genStmt alloc (Return expr _) = genExp alloc expr (AReg 0)
genStmt (varMap,n) (Asgn var oper expr srcPos) = let
  -- Look up identity's index
  l = ATemp $ varMap Map.! var
  -- If there is an operation, construct the appropriate expression
  expr' = case oper of
         Nothing -> expr
         Just op -> ExpBinOp op (Ident var srcPos) expr srcPos
  in genExp (varMap,n) expr' l

-- Generates AAsm from an expression
genExp :: Alloc -> Expr -> ALoc -> [AAsm]
genExp _ (ExpInt n _) l = [AAsm [l] Nop [AImm $ fromIntegral n]]
genExp (varMap,_) (Ident s _) l = [AAsm [l] Nop [ALoc $ ATemp $ varMap Map.! s]]
genExp (varMap,n) (ExpBinOp op e1 e2 _) l = let
  -- AAsm for left and right operand
  -- TODO: Make this more SSL friendly
  i1 = genExp (varMap, n + 1) e1 (ATemp n)
  i2 = genExp (varMap, n + 2) e2 (ATemp $ n + 1)
  -- AAsm for the operation
  c  = [AAsm [l] op [ALoc $ ATemp n, ALoc $ ATemp $ n + 1]]
  in i1 ++ i2 ++ c
genExp (varMap,n) (ExpUnOp op e _) l = let
  -- AAsm for operand
  i1 = genExp (varMap, n + 1) e (ATemp n)
  -- AAsm for the operation
  c  = [AAsm [l] op [ALoc $ ATemp n]]
  in i1 ++ c
