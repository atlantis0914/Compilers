{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Currently just a pseudolanguage with 3-operand instructions and arbitrarily many temps.
-}
module Compile.Backend.CodeGen where

import Compile.Types
import qualified Data.Map as Map

import Compile.IR.GenIR
import Compile.IR.InlineFn
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
import Compile.Backend.Neededness
import Compile.Backend.RegisterCoal

import Compile.Util.Job

import Compile.Frontend.ConstantPropagate

import qualified Debug.Trace as Trace

type Alloc = (Map.Map String Int, Int)

debugFlag = False

maxTempsBeforeSpilling = 1000

-- fnListCodeGen :: FnList -> FnMap -> String
fnListCodeGen job fnList fnMap =
  let
    safeCompilation = jobSafeCompilation job
    fnAasms = genFIR job fnList fnMap safeCompilation
    fnAasms' = if (optLevelMet job inliningOptLevel)
                 then inlineFns fnMap fnAasms
                 else fnAasms
    asm = concatMap (fnAAsmCodeGen job) fnAasms'
    epilogue = concat ["error:\n", "  movw $1, %ax\n", "  movw $0, %bx\n", "  divw %bx\n", "mem_error:\n", "  jmp 0\n"]
    asm' = asm ++ epilogue
  in
    asm'

genFnProlugues :: Int -> Int -> String
genFnProlugues numArgs m =
  let
    pushBP = if numArgs > 6 then "  pushq %rbp\n  movq %rsp, %rbp\n"
                            else ""
    callees' = take (max 0 (m - 6)) callees
    rest = concatMap genPrologueIns callees'
    n = if numArgs > 6 then (length callees') + 1
                       else length callees'
    buffer = if n `mod` 2 == 0 then decrStack8
                               else ""
  in
    pushBP ++ rest ++ buffer

fnAAsmCodeGen :: Job -> FnAAsm -> String
fnAAsmCodeGen job (AAFDefn aasms fnName numArgs) =
    concat (prologue ++ [asms])
  where
    (asms, size, m) = codeGen aasms fnName numArgs job
    prologue = [".globl " ++ fnName ++ "\n", fnName ++ ":\n", genFnProlugues numArgs m, substr]
    substr = if (size > 0)
               then "  subq $" ++ show size ++ ", %rsp\n"
               else ""

fnAAsmCodeGen _ (AAFDecl fnName) = ""

maxColor :: ColoringMap -> Int
maxColor coloring = Map.foldl maxColor' 0 coloring

maxColor' :: Int -> Color -> Int
maxColor' m (Color c) = max m c

mergeAAsm :: (AAsm, [ALoc]) -> AAsm
mergeAAsm (AFnCall fnName loc locs _, lives) = AFnCall fnName loc locs lives
mergeAAsm (aasm, _) = aasm

-- Generates the AAsm from an AST
codeGen aasmList fnName numArgs job = let
    allLocs = getLocs aasmList
  in
    if (length (aasmList) > maxTempsBeforeSpilling)
      then (let
             twoOpAasmList =  genTwoOperand aasmList
             coloring = naiveColor allLocs
             m = maxColor coloring
             m' = (max 0 (m - max_color_num)) * 8
             m'' = roundUp m'
             coloredAasmList = colorTemps twoOpAasmList coloring
             asm = genAsm coloredAasmList (fnName, m'', numArgs, m) job
           in
             (concat asm, m'', m))
      else (let
              aasmList' = if (optLevelMet job deadCodeOptLevel) 
                            then removeDead aasmList
                            else aasmList
              twoOpAasmList =  genTwoOperand aasmList'
              liveVars = liveness twoOpAasmList
              interference_graph@(Graph gmap) = buildInterferenceGraph twoOpAasmList liveVars
              simp_ordering = maximumCardinalitySearch interference_graph -- now a [Vertex ALoc]
              coloring = greedyColor interference_graph simp_ordering
              liveVars' = map (\l -> map (replaceAssigns coloring) l) liveVars
              twoOpAasmList' = map mergeAAsm (zip twoOpAasmList liveVars')
              (twoOpAasmList'', coloring') = if (optLevelMet job regCoalesceOptLevel)
                                               then registerCoalesce twoOpAasmList' coloring interference_graph
                                               else (twoOpAasmList', coloring)
              m = maxColor coloring'
              m' = (max 0 (m - max_color_num)) * 8
              m'' = roundUp m'
              coloredAasmList = colorTemps twoOpAasmList'' coloring'
              asm = genAsm coloredAasmList (fnName, m'', numArgs, m) job
            in
              if (debugFlag)
                then (genDebug aasmList liveVars interference_graph simp_ordering coloring twoOpAasmList coloredAasmList (concat asm), m'', m)
                else (concat asm, m'', m))

genDebug aasm liveVars (Graph gmap) simp_ord coloring twoOpAasm coloredAasm asm =
  let
    aasm' = listShow aasm
    liveVars' = listShow liveVars
    gmap' = listShow $ Map.toList gmap
    coloring' = listShow $ Map.toList coloring
    simp_ord' = listShow simp_ord
    twoOpAasm' = listShow twoOpAasm
    coloredAasm' = listShow coloredAasm
    asm' = asm
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
