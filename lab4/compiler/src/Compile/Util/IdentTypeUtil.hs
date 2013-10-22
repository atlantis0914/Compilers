module Compile.Util.IdentTypeUtil where 

import Compile.Types

import qualified Data.Map as Map

toIdentType :: String -> IdentType
toIdentType "int" = IInt
toIdentType "bool" = IBool
toIdentType "void" = IVoid
toIdentType s = ITypeDef s

tM = [(IInt, IInt),
      (IBool, IBool),
      (IVoid, IVoid)]

baseIdentTypeMap = Map.fromList tM
