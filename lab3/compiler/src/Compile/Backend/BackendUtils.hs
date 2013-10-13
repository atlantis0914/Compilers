module Compile.Backend.BackendUtils where

import Compile.Types
import qualified Data.Map as Map
import Compile.Backend.Registers

genPrologueIns reg =
  "  pushq " ++ reg ++ "\n"

genEpilogueIns reg =
  "  popq " ++ reg ++ "\n"

