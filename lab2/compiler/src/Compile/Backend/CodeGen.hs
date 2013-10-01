{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Currently just a pseudolanguage with 3-operand instructions and arbitrarily many temps.
-}
module Compile.Backend.CodeGen where

import Compile.Types
import qualified Data.Map as Map


import Compile.IR.GenIR
import Compile.Backend.Liveness
import Compile.Backend.Interference
import Compile.Backend.Coloring
import Compile.Backend.ColorTemp
import Compile.Backend.GenTwoOperand
import Compile.Backend.MaximumCardinalitySearch
import Compile.Backend.ColorTemp
import Compile.Backend.GenAsm
import Compile.Backend.Spill

import qualified Debug.Trace as Trace

type Alloc = (Map.Map String Int, Int)

-- codeGen ast@(AST (Block stmts) _) = let
--   ir = genIR ast
--   in
--     concatMap (\a -> (show a) ++ "\n") ir

debugFlag = False

maxTempsBeforeSpilling = 600

-- Generates the AAsm from an AST
codeGen ast@(AST (Block stmts) _) = let
    aasmList = genIR ast
--    twoOpAasmList = genTwoOperand aasmList
    allLocs = getLocs aasmList
  in
    if (length (aasmList) > maxTempsBeforeSpilling)
      then (let
             coloring = naiveColor allLocs
             coloredAasmList = colorTemps aasmList coloring
             twoOperandAasmList = (genTwoOperand coloredAasmList)
             spilledAasmList = spill twoOperandAasmList
             asm = genAsm spilledAasmList
           in
             concat asm)
      else (let
              liveVars = liveness aasmList
              interference_graph@(Graph gmap) = buildInterferenceGraph aasmList liveVars
              simp_ordering = maximumCardinalitySearch interference_graph -- now a [Vertex ALoc]
              coloring = greedyColor interference_graph simp_ordering
              coloredAasmList = colorTemps aasmList coloring
              twoOperandAasmList = (genTwoOperand coloredAasmList)
              spilledAasmList = spill twoOperandAasmList
              asm = genAsm spilledAasmList
            in
              if (debugFlag)
                then genDebug stmts aasmList liveVars interference_graph simp_ordering coloring twoOperandAasmList coloredAasmList asm
                else concat asm)

genDebug stmts aasm liveVars (Graph gmap) simp_ord coloring twoOpAasm coloredAasm asm =
  let
    stmts' = listShow stmts
    aasm' = listShow aasm
    liveVars' = listShow liveVars
    gmap' = listShow $ Map.toList gmap
    coloring' = listShow $ Map.toList coloring
    simp_ord' = listShow simp_ord
    twoOpAasm' = listShow twoOpAasm
    coloredAasm' = listShow coloredAasm
    asm' = listShow asm
  in
    "Statements\n" ++ stmts' ++ "\n\n" ++
    "Aasm\n" ++ aasm' ++ "\n\n" ++
    "LiveVars\n" ++ liveVars' ++ "\n\n" ++
    "InterferenceGraph\n" ++ gmap' ++ "\n\n" ++
    "Simp Ordering\n" ++ simp_ord' ++ "\n\n" ++
    "Coloring\n" ++ coloring' ++ "\n\n" ++
    "TwoOpAAsm\n" ++ twoOpAasm' ++ "\n\n" ++
    "ColoredAAsm\n" ++ coloredAasm' ++ "\n\n" ++
    "Asm\n" ++ asm' ++ "\n\n"

listShow l = concat (map (\a -> (show a) ++ "\n") l)
-- 
-- -- Modify to just find the index of the first return, and
-- -- take that many statments.
-- dropAfterFirstReturn :: [Stmt] -> [Stmt]
-- dropAfterFirstReturn stmts =
--   let
--     f = \(acc, st) ->
--         \stmt ->
--           case (st,stmt) of
--             (Nothing, Ctrl (Return _ _)) -> (acc ++ [stmt], Just ())
--             (Nothing, _) -> (acc ++ [stmt], Nothing)
--             (Just _, _) -> (acc, st)
--   in
--     fst $ foldl f ([], Nothing) stmts
-- 
-- -- Generates AAsm from a statement
-- genStmt :: Alloc -> Stmt -> [AAsm]
-- genStmt alloc (Ctrl (Return expr _)) = genExp alloc expr (AReg 0)
-- genStmt alloc (Decl _ _ _ Nothing) = []
-- genStmt (varMap, n) (Decl _ _ _ (Just (Asgn var oper expr srcPos))) = let
--   l = ATemp $ varMap Map.! var
--   expr' = case oper of
--           Nothing -> expr
--           Just op -> error "Can't have a binOP in a decl"
--   in genExp (varMap, n) expr' l
-- 
-- genStmt (varMap,n) (Asgn var oper expr srcPos) = let
--   -- Look up identity's index
--   l = ATemp $ varMap Map.! var
--   -- If there is an operation, construct the appropriate expression
--   expr' = case oper of
--          Nothing -> expr
--          Just op -> ExpBinOp op (Ident var srcPos) expr srcPos
--   in genExp (varMap,n) expr' l
-- 
-- -- Generates AAsm from an expression
-- genExp :: Alloc -> Expr -> ALoc -> [AAsm]
-- genExp _ (ExpInt n _ _) l = [AAsm [l] Nop [AImm $ fromIntegral n]]
-- genExp (varMap,_) (Ident s _) l = [AAsm [l] Nop [ALoc $ ATemp $ varMap Map.! s]]
-- 
-- genExp (varMap,n) (ExpBinOp op e1 e2 _) l = let
--   -- AAsm for left and right operand
--   -- TODO: Make this more SSL friendly
--   i1 = genExp (varMap, n + 1) e1 (ATemp n)
--   i2 = genExp (varMap, n + 2) e2 (ATemp $ n + 1)
--   -- AAsm for the operation
--   c  = [AAsm [l] op [ALoc $ ATemp n, ALoc $ ATemp $ n + 1]]
--   in i1 ++ i2 ++ c
-- 
-- genExp (varMap,n) (ExpUnOp op e _) l = let
--   -- AAsm for operand
--   i1 = genExp (varMap, n + 1) e (ATemp n)
--   -- AAsm for the operation
--   c  = [AAsm [l] op [ALoc $ ATemp n]]
--   in i1 ++ c
