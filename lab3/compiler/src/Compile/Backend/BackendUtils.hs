module Compile.Backend.BackendUtils where

import Compile.Types
import qualified Data.Map as Map
import Compile.Backend.Registers

genPrologueIns reg =
  "  pushq " ++ reg ++ "\n"

moveStack :: (String, Int) -> String -> (String, Int)
moveStack (asms, i) reg =
  let
    next = "  movq " ++ reg ++ ", -" ++ show ((i + 1) * 8) ++ "(%rsp)\n"
  in
    (asms ++ next, i + 1)

genEpilogueIns reg =
  "  popq " ++ reg ++ "\n"

roundUp n =
  if n `mod` 16 == 0 then n
                     else n + (16 - (n `mod` 16))

incrStack8 = "  addq $8, %rsp\n"
decrStack8 = "  subq $8, %rsp\n"
