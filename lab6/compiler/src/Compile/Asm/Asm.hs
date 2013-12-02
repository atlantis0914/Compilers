module Compile.Asm.Asm where 

import Compile.Types
import Compile.Asm.FnAsm
import Compile.Asm.StructAsm
import System.FilePath
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

toAsm :: Job -> IRFnList -> String
toAsm job (IRFnList decls) = 
  let
    structMap = genStructMap decls
    moduleName = takeFileName $ jobSource job
    prologue = genAsmPrologue "c0module" 
    structAccessors = genStructAccessorsFromMap structMap
    fns = concatMap (\x -> (genAsmIRDecl x) ++ "\n") decls   
    epilogue = genAsmEpilogue 
  in
    genUtility ++ prologue ++ structAccessors ++ fns ++ epilogue ++ genModule

polyFillMul :: String
polyFillMul = 
  "function polyMul(a, b) {\n" ++
  "  var ah  = (a >>> 16) & 0xffff;\n" ++
  "  var al = a & 0xffff;\n" ++
  "  var bh  = (b >>> 16) & 0xffff;\n" ++
  "  var bl = b & 0xffff;\n" ++
  "  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);\n" ++
  "}\n"

genUtility :: String 
genUtility = 
  polyFillMul ++ 
  "if (typeof (this.Math.imul) == \"undefined\") {\n" ++ 
  "  this.Math.imul = polyMul\n" ++ 
  "}\n"

genModule :: String
genModule =   
  "var c0_export = c0module(this, {}, new Int32Array(4096));\n" ++ 
  "var res = (c0_export.main())\n" ++ 
  "var numEx = (c0_export.getNumEx())\n"


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
  "  var g_numex = 0;" ++ "\n" ++ 
  "  var g_assertex = 0;" ++ "\n" ++ 
  "  var imul = stdlib.Math.imul" ++ 
  "\n\n" ++ 
  "  // Function Declarations" ++ 
  "\n\n" ++ 
  genMemAllocator ++ 
  "\n\n" ++ 
  genStackInitialization ++ 
  "\n\n" ++ 
  genPolyDiv ++ 
  "\n\n" ++ 
  genPolyMod ++ 
  "\n\n" ++ 
  genPointerDeref ++ 
  "\n\n" 
  

genAsmEpilogue :: String 
genAsmEpilogue = 
  "  function main() {" ++ "\n" ++ 
  "    stackInit();" ++ "\n" ++ 
  "    return _c0_main() | 0;" ++ "\n" ++ 
  "  }" ++ "\n\n" ++ 
  "  function getNumEx() {\n" ++ 
  "    return g_numex | 0;\n" ++ 
  "  }\n" ++
  "  function getMemEx() {\n" ++ 
  "    return g_memex | 0;\n" ++ 
  "  }\n" ++
  "  return { main : main , getNumEx : getNumEx, getMemEx : getMemEx }" ++ "\n\n" ++ 
  "}\n"

genAsmIRDecl :: IRDecl -> String
genAsmIRDecl (IRFDefn fdef) =  
  genAsmFnDecl fdef

genAsmIRDecl (IRSDefn (IRStructDef sName sFlds sTyps sOffs _ sSz)) = ""

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

genPolyDiv :: String
genPolyDiv = 
  "  function polyDiv(num, den) {\n" ++ 
  "    num = num | 0;\n" ++ 
  "    den = den | 0;\n" ++ 
  "    var ret = 0;\n" ++ 
  "    ret = (num | 0) / (den | 0)| 0;\n" ++ 
  "    if ((den | 0) == (0 | 0)) {\n" ++ 
  "      g_numex = 1;\n" ++ 
  "    }\n" ++ 
  "    return ret | 0;\n" ++ 
  "  }\n"

genPolyMod :: String
genPolyMod = 
  "  function polyMod(num, den) {\n" ++ 
  "    num = num | 0;\n" ++ 
  "    den = den | 0;\n" ++ 
  "    var ret = 0;\n" ++ 
  "    ret = (num | 0) % (den | 0)| 0;\n" ++ 
  "    if ((den | 0) == (0 | 0)) {\n" ++ 
  "      g_numex = 1;\n" ++ 
  "    }\n" ++ 
  "    return ret | 0;\n" ++ 
  "  }\n"

genPointerDeref :: String
genPointerDeref = 
  "  function pointerDeref(loc) {\n" ++ 
  "    loc = loc | 0;\n" ++ 
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    return H32[(loc | 0) >> 2] | 0;\n" ++ 
  "  }"

