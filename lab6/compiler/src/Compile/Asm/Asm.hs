module Compile.Asm.Asm where 

import Compile.Types
import Compile.Asm.FnAsm
import Compile.Asm.StructAsm
import Compile.Asm.LibraryAsm
import System.FilePath
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

toAsm :: Job -> IRFnList -> String
toAsm job (IRFnList decls) = 
  let
--    structMap = genStructMap decls
    libraryasm = generateLibraryAsm 
    moduleName = takeFileName $ jobSource job
    prologue = genAsmPrologue "c0module" 
--    structAccessors = genStructAccessorsFromMap structMap
    fns = concatMap (\x -> (genAsmIRDecl x) ++ "\n") decls   
    epilogue = genAsmEpilogue 
  in
    libraryasm ++ genUtility ++ prologue ++ fns ++ epilogue ++ genModule

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
  "var c0arr = new Int32Array(5000000)\n" ++ 
  "c0arr[0] = 5000000;\n" ++ 
  "var c0_export = c0module(this, foreignImports, c0arr);\n" ++ 
  "var res = (c0_export.main())\n" ++ 
  "var numEx = (c0_export.getNumEx())\n" ++
  "var memEx = (c0_export.getMemEx())\n" ++
  "var assEx = (c0_export.getAssEx())\n" ++
  "print(\"Result: \" + res)\n" ++
  "print(\"NumEx: \" + numEx)\n" ++
  "print(\"MemEx: \" + memEx)\n" ++ 
  "print(\"AssEx: \" + assEx)"

genAsmPrologue :: String -> String
genAsmPrologue moduleName = 
  "function " ++ moduleName ++ "(stdlib, foreign, heap) {\n" ++ 
  "  \"use asm\";" ++ 
  "\n\n" ++ 
  "  // Global Declarations" ++ "\n" ++ 
  "  var H32 = new stdlib.Int32Array(heap);" ++ "\n" ++ 
  "  var g_heapoff = 1;" ++ "\n" ++ 
  "  var g_stackoff = 0;" ++
  "\n\n" ++ 
  "  // Global Exception Catchers" ++ "\n" ++ 
  "  var g_memex = 0;" ++ "\n" ++ 
  "  var g_oomex = 0;" ++ "\n" ++ 
  "  var g_numex = 0;" ++ "\n" ++ 
  "  var g_assex = 0;" ++ "\n" ++ 
  "  var imul = stdlib.Math.imul;" ++ "\n" ++ 
  "  var INTMIN = 0x80000000;" ++ 
  "\n\n" ++ 
  "  // Function Declarations" ++ 
  "\n\n" ++ 
  genAsmBindings ++ 
  "\n\n" ++ 
  genMemAllocator ++ 
  "\n\n" ++ 
  genMemArrAllocator ++ 
  "\n\n" ++ 
  genStackInitialization ++ 
  "\n\n" ++ 
  genPolyDiv ++ 
  "\n\n" ++ 
  genPolyMod ++ 
  "\n\n" ++ 
  genPolyRShift ++
  "\n\n" ++ 
  genPolyLShift ++
  "\n\n" ++ 
  genPointerDeref ++ 
  "\n\n" ++ 
  genPointerLoad ++
  "\n\n" ++ 
  genGenericAccessor ++ 
  "\n\n" ++ 
  genArrAccessor ++ 
  "\n\n" ++ 
  genGenericFieldShift ++ 
  "\n\n" ++ 
  genGenericArrShift ++ 
  "\n\n" ++ 
  genGenericMemSet ++ 
  "\n\n" ++ 
  genAssert ++ 
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
  "  function getOomEx() {\n" ++ 
  "    return g_oomex | 0;\n" ++ 
  "  }\n" ++
  "  function getAssEx() {\n" ++ 
  "    return g_assex | 0;\n" ++ 
  "  }\n" ++
  "  return { main : main , getNumEx : getNumEx, getMemEx : getMemEx, getOomEx : getOomEx, getAssEx : getAssEx}" ++ "\n\n" ++ 
  "}\n"

genAsmIRDecl :: IRDecl -> String
genAsmIRDecl (IRFDefn fdef) =  
  genAsmFnDecl fdef

genAsmIRDecl sdef@(IRSDefn (IRStructDef sName sFlds sTyps sOffs _ sSz)) = ""

genMemAllocator :: String
genMemAllocator = 
  "  function memAlloc(size) {" ++ "\n" ++
  "    size = size | 0;" ++ "\n" ++ 
  "    var ret = 0" ++  "\n" ++ 
  "    if ((g_heapoff | 0) < (H32[0 | 0] | 0)) {" ++ "\n" ++ 
  "      ret = g_heapoff | 0;" ++ "\n" ++ 
  "      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;" ++ "\n" ++
  "    } else {" ++ "\n" ++ 
  "      g_oomex = 1 | 0;\n" ++
  "    }\n" ++
  "   return ret | 0;" ++ "\n" ++ 
  "  }"

genMemArrAllocator :: String
genMemArrAllocator = 
  "  function memArrAlloc(size, numElems) {" ++ "\n" ++
  "    size = size | 0;" ++ "\n" ++ 
  "    numElems = numElems | 0;" ++ "\n" ++ 
  "    size = (size | 0) + (1 | 0) | 0;" ++ "\n" ++ 
  "    numElems = (size | 0) | 0;" ++ "\n" ++ 
  "    var ret = 0;" ++  "\n" ++ 
  "    if ((g_heapoff | 0) < (H32[0 | 0] | 0)) {" ++ "\n" ++ 
  "      ret = g_heapoff | 0;" ++ "\n" ++ 
  "      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;" ++ "\n" ++
  "    } else {" ++ "\n" ++ 
  "      g_oomex = 1 | 0;\n" ++
  "    }\n" ++
  "    if (((numElems | 0) - (1 | 0) | 0) < (0 | 0)) {" ++ "\n" ++ 
  "      g_memex = 1 | 0;\n" ++
  "    }\n" ++
  "    H32[ret | 0] = ((numElems | 0) - (1 | 0)) | 0;\n" ++
  "   return ret | 0;" ++ "\n" ++ 
  "  }"

genStackInitialization :: String
genStackInitialization = 
  "  function stackInit() {" ++ "\n" ++
  "    g_stackoff = (H32[0 | 0] | 0) - 1 | 0;" ++ "\n" ++ 
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
  "    if (((num | 0) == (INTMIN | 0)) && ((den | 0) == (-1 | 0))) {\n" ++ 
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
  "    if (((num | 0) == (INTMIN | 0)) && ((den | 0) == (-1 | 0))) {\n" ++ 
  "      g_numex = 1;\n" ++ 
  "    }\n" ++ 
  "    return ret | 0;\n" ++ 
  "  }\n"

genPolyLShift :: String
genPolyLShift = 
  "  function polyLShift(l, r) {\n" ++ 
  "    l = l | 0;\n" ++ 
  "    r = r | 0;\n" ++ 
  "    var ret = 0;\n" ++ 
  "    ret = (l | 0) << (r | 0) | 0;\n" ++ 
  "    if ((r | 0) >= (32 | 0)) {\n" ++ 
  "      g_numex = 1;\n" ++ 
  "    }\n" ++ 
  "    if ((r | 0) < (0 | 0)) {\n" ++ 
  "      g_numex = 1;\n" ++ 
  "    }\n" ++ 
  "    return ret | 0;\n" ++ 
  "  }\n"

genPolyRShift :: String
genPolyRShift = 
  "  function polyRShift(l, r) {\n" ++ 
  "    l = l | 0;\n" ++ 
  "    r = r | 0;\n" ++ 
  "    var ret = 0;\n" ++ 
  "    ret = (l | 0) >> (r | 0) | 0;\n" ++ 
  "    if ((r | 0) >= (32 | 0)) {\n" ++ 
  "      g_numex = 1;\n" ++ 
  "    }\n" ++ 
  "    if ((r | 0) < (0 | 0)) {\n" ++ 
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
  "    return loc | 0;\n" ++ 
  "  }"

genPointerLoad :: String
genPointerLoad = 
  "  function pointerLoad(loc) {\n" ++ 
  "    loc = loc | 0;\n" ++ 
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    return H32[loc | 0] | 0;\n" ++ 
  "  }"

genGenericAccessor :: String
genGenericAccessor = 
  "  function fieldAccess(loc, off) {\n" ++ 
  "    loc = loc | 0;\n" ++ 
  "    off = off | 0;\n" ++ 
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    return H32[loc + off | 0] | 0;\n" ++ 
  "  }\n"

genArrAccessor :: String
genArrAccessor = 
  "  function arrAccess(loc, off) {\n" ++ 
  "    loc = loc | 0;\n" ++ 
  "    off = off | 0;\n" ++ 
  "    if ((off | 0) < (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    if ((off | 0) >= (H32[loc | 0] | 0)) {\n" ++ 
  "      g_memex = 1 | 0;\n" ++ 
  "    }\n" ++ 
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    return H32[loc + off + (1 | 0)] | 0;\n" ++ 
  "  }\n"

genGenericFieldShift :: String
genGenericFieldShift = 
  "  function fieldShift(loc, off) {\n" ++
  "    loc = loc | 0;\n" ++
  "    off = off | 0;\n" ++
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    return (loc | 0) + (off | 0) | 0;\n" ++
  "  }\n"

genGenericArrShift :: String
genGenericArrShift = 
  "  function arrShift(loc, off) {\n" ++
  "    loc = loc | 0;\n" ++
  "    off = off | 0;\n" ++
  "    if ((off | 0) < (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    if ((off | 0) >= (H32[loc | 0] | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    return (loc | 0) + (off | 0) + (1 | 0) | 0;\n" ++
  "  }\n"
 
genGenericMemSet :: String
genGenericMemSet = 
  "  function memSet(loc, val) {\n" ++ 
  "    loc = loc | 0;\n" ++ 
  "    val = val | 0;\n" ++ 
  "    if ((loc | 0) == (0 | 0)) {\n" ++ 
  "      g_memex = 1;\n" ++ 
  "    }\n" ++ 
  "    H32[loc | 0] = val | 0;\n" ++ 
  "    return;\n" ++ 
  "  }\n"

genAssert :: String
genAssert = 
  "  function assert(e) {\n" ++ 
  "    e = e | 0\n" ++ 
  "    if ((e | 0) != (1 | 0)) {\n" ++ 
  "      g_assex = 1;\n" ++ 
  "    }\n" ++ 
  "    return;\n" ++ 
  "  }\n"
  
