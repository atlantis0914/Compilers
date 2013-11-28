module FnAsm where 

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace


genAsmFnDecl :: IRFuncDef -> String 
genAsmFnDecl (IRFuncDef name args argTypes retType body argSizes) = 
  
