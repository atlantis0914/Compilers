module Compile.Asm.LibraryAsm where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

genAsmBindings :: String
genAsmBindings = 
  "  var fadd = foreign.fadd;\n" ++
  "  var fsub = foreign.fsub;\n" ++
  "  var fmul = foreign.fmul;\n" ++
  "  var fdiv = foreign.fdiv;\n" ++
  "  var fless = foreign.fless;\n" ++
  "  var itof = foreign.itof;\n" ++
  "  var ftoi = foreign.ftoi;\n" ++
  "  var print_fpt = foreign.print_fpt;\n" ++
  "  var print_int = foreign.print_int;\n" ++
  "  var print_hex = foreign.print_hex;"

generateLibraryAsm :: String
generateLibraryAsm = 
  genFadd ++ "\n\n" ++ 
  genFsub ++ "\n\n" ++ 
  genFmul ++ "\n\n" ++ 
  genFdiv ++ "\n\n" ++ 
  genFless ++ "\n\n" ++ 
  genItof ++ "\n\n" ++ 
  genFtoi ++ "\n\n" ++
  genPrintfpt ++ "\n\n" ++
  genPrintint ++ "\n\n" ++ 
  genPrinthex ++ "\n\n" ++ 
  foreignObj ++ "\n\n"

foreignObj :: String
foreignObj = 
  "foreignImports = {\n" ++
  "  fadd : fadd,\n" ++
  "  fsub : fsub,\n" ++
  "  fmul : fmul,\n" ++
  "  fdiv : fdiv,\n" ++
  "  fless : fless,\n" ++
  "  itof : itof,\n" ++
  "  ftoi : ftoi,\n" ++
  "  print_fpt : print_fpt,\n" ++
  "  print_int : print_int,\n" ++
  "  print_hex : print_hex\n" ++
  "}"


genFadd :: String
genFadd =
  "function fadd(x, y) {\n" ++
  "  return x + y;\n" ++
  "}"

genFsub :: String
genFsub =
  "function fsub(x, y) {\n" ++
  "  return x - y;\n" ++
  "}"

genFmul :: String
genFmul =
  "function fmul(x, y) {\n" ++
  "  return x * y;\n" ++
  "}"

genFdiv :: String
genFdiv =
  "function fdiv(x, y) {\n" ++
  "  return x / y;\n" ++
  "}"

genFless :: String
genFless =
  "function fless(x, y) {\n" ++
  " return (x < y);\n" ++
  "}"

genItof :: String
genItof = 
  "function itof(x) {\n" ++
  "  return x;\n" ++
  "}"

genFtoi :: String
genFtoi = 
  "function ftoi(x) {\n" ++
  "  return x;\n" ++
  "}"

genPrintfpt :: String
genPrintfpt = 
  "function print_fpt(x) {\n" ++
  "  return x;\n" ++
  "}"

genPrintint :: String
genPrintint = 
  "function print_int(x) {\n" ++
  "  return x;\n" ++
  "}"

genPrinthex :: String
genPrinthex = 
  "function print_hex(x) {\n" ++
  "  return x;\n" ++
  "}"
