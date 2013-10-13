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
import Compile.Backend.BackendUtils
import Compile.Backend.Registers

import qualified Debug.Trace as Trace

type Alloc = (Map.Map String Int, Int)

debugFlag = False

maxTempsBeforeSpilling = 600

fnListCodeGen :: FnList -> String
fnListCodeGen fnList =
  let
    fnAasms = genFIR fnList
    asm = concatMap fnAAsmCodeGen fnAasms
    epilogue = concat ["error:\n", "movw $1, %ax\n", "movw $0, %bx\n", "divw %bx\n"]
  in
    asm ++ epilogue

genFnProlugues :: String
genFnProlugues = concatMap genPrologueIns callees

fnAAsmCodeGen :: FnAAsm -> String
fnAAsmCodeGen (AAFDefn aasms fnName) =
  let
    (asms, size) = codeGen aasms fnName
    prologue = [".globl " ++ fnName ++ "\n", fnName ++ ":\n", "  pushq %rbp\n", "  movq %rsp, %rbp\n", genFnProlugues, "  subq $" ++ show size ++ ", %rsp\n"]
  in
    concat (prologue ++ [asms])

fnAAsmCodeGen (AAFDecl fnName) =
  ""

maxColor :: ColoringMap -> Int
maxColor coloring = Map.foldl maxColor' 0 coloring

maxColor' :: Int -> Color -> Int
maxColor' m (Color c) = max m c

-- Generates the AAsm from an AST
codeGen aasmList fnName = let
    twoOpAasmList =  genTwoOperand aasmList
    allLocs = getLocs aasmList
  in
    if (length (aasmList) > maxTempsBeforeSpilling)
      then (let
             coloring = naiveColor allLocs
             m = maxColor coloring
             m' = (max 0 (m - max_color_num)) * 8
             coloredAasmList = colorTemps twoOpAasmList coloring
             asm = genAsm coloredAasmList (fnName, m')
           in
             (concat asm, m'))
      else (let
              liveVars = liveness twoOpAasmList
              interference_graph@(Graph gmap) = buildInterferenceGraph twoOpAasmList liveVars
              simp_ordering = maximumCardinalitySearch interference_graph -- now a [Vertex ALoc]
              coloring = greedyColor interference_graph simp_ordering
              m = maxColor coloring
              m' = (max 0 (m - max_color_num)) * 8
              coloredAasmList = colorTemps twoOpAasmList coloring
              asm = genAsm coloredAasmList (fnName, m')
            in
              if (debugFlag)
                then (genDebug aasmList liveVars interference_graph simp_ordering coloring twoOpAasmList coloredAasmList asm, m')
                else (concat asm, m'))

genDebug aasm liveVars (Graph gmap) simp_ord coloring twoOpAasm coloredAasm asm =
  let
    aasm' = listShow aasm
    liveVars' = listShow liveVars
    gmap' = listShow $ Map.toList gmap
    coloring' = listShow $ Map.toList coloring
    simp_ord' = listShow simp_ord
    twoOpAasm' = listShow twoOpAasm
    coloredAasm' = listShow coloredAasm
    asm' = listShow asm
  in
    "Aasm\n" ++ aasm' ++ "\n\n" ++
    "LiveVars\n" ++ liveVars' ++ "\n\n" ++
    "InterferenceGraph\n" ++ gmap' ++ "\n\n" ++
    "Simp Ordering\n" ++ simp_ord' ++ "\n\n" ++
    "Coloring\n" ++ coloring' ++ "\n\n" ++
    "TwoOpAAsm\n" ++ twoOpAasm' ++ "\n\n" ++
    "ColoredAAsm\n" ++ coloredAasm' ++ "\n\n" ++
    "Asm\n" ++ asm' ++ "\n\n"

listShow l = concat (map (\a -> (show a) ++ "\n") l)
