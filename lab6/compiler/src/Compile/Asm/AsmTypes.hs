module Compile.Asm.AsmTypes where 

import Control.Monad.State
import qualified Data.Map as Map

type InnerState = (String, Int, Map.Map String Int)
type AsmState = State InnerState
