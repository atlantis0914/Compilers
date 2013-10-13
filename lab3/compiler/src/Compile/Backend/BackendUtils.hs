module Compile.Backend.BackendUtils where

import Compile.Types
import qualified Data.Map as Map
import Compile.Backend.Registers

genPrologueIns reg =
  "  pushl " ++ reg ++ "\n"

genEpilogueIns reg =
  "  popl " ++ reg ++ "\n"

