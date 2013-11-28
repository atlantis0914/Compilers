module Compile.Asm.Asm where 

import Compile.Types
-- import Compile.Asm.FnAsm
-- import Compile.Asm.StructAsm
import System.FilePath
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

toAsm :: Job -> IRFnList -> String
toAsm job (IRFnList decls) = 
  let
    moduleName = takeFileName $ jobSource job
    prologue = genAsmPrologue "c0module" 
    fns = concatMap (\x -> (genAsmIRDecl x) ++ "\n") decls   
    epilogue = genAsmEpilogue 
  in
    prologue ++ fns ++ epilogue


genAsmPrologue :: String -> String
genAsmPrologue moduleName = 
  "function " ++ moduleName ++ "(stdlib, foreign, heap) {\n" ++ 
  "  \"use asm\";" ++ 
  "\n\n" ++ 
  "  // Global Declarations" ++ "\n" ++ 
  "  var H32 = new stdlib.Int32Array(heap);" ++ "\n" ++ 
  "  var g_heapoff = 0;" ++ "\n" ++ 
  "  var g_stackoff = 0;" ++
  "\n\n" ++ 
  "  // Global Exception Catchers" ++ "\n" ++ 
  "  var g_memex = 0;" ++ "\n" ++ 
  "  var g_numex = 0;" ++ 
  "\n\n" ++ 
  "  // Function Declarations" ++ 
  "\n\n" ++ 
  genMemAllocator ++ 
  "\n\n" ++ 
  genStackInitialization ++ 
  "\n\n"
  

genAsmEpilogue :: String 
genAsmEpilogue = 
  "  function main() {" ++ "\n" ++ 
  "    stackInit();" ++ "\n" ++ 
  "    return 1 | 0;" ++ "\n" ++ 
  "  }" ++ "\n\n" ++ 
  "  return { main : main }" ++ "\n\n" ++ 
  "}"

genAsmIRDecl :: IRDecl -> String
genAsmIRDecl (IRFDefn (IRFuncDef name args argTypes retTypes body argSizes)) =  ""
genAsmIRDecl (IRSDefn (IRStructDef sName sFlds sTyps sOffs _ sSz)) = ""

genStructAccessor :: String -> String -> Int -> String
genStructAccessor pref fieldName off = 
  "function " ++ pref ++ "_" ++ fieldName  ++ "(base) {" ++ "\n" ++
  "  base = base | 0;" ++ "\n" ++ 
  "  return H32[base + " ++ (show off) ++ " >> 2] | 0;" ++ "\n" ++ 
  "}"

genMemAllocator :: String
genMemAllocator = 
  "  function memAlloc(size) {" ++ "\n" ++
  "    size = size | 0;" ++ "\n" ++ 
  "    if ((g_heapoff | 0) < (g_stackoff | 0)) {" ++ "\n" ++ 
  "      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;" ++ "\n" ++
  "    }" ++ "\n" ++ 
  "  }"

genStackInitialization :: String
genStackInitialization = 
  "  function stackInit() {" ++ "\n" ++
  "    g_stackoff = (H32[0 >> 2] | 0) - 1 | 0;" ++ "\n" ++ 
  "  }"
