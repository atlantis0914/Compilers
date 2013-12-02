module Compile.Asm.AsmTypes where 

import Compile.Types
import Control.Monad.State
import qualified Data.Map as Map

type InnerState = (String, Int, Map.Map String Int)
type AsmState = State InnerState

type StructMap = Map.Map String IRStructDef
