module Compile.Util.IdentTypeUtil where 

import Compile.Types

toIdentType :: String -> IdentType
toIdentType "int" = IInt
toIdentType "bool" = IBool
toIdentType "void" = IVoid
toIdentType s = ITypeDef s
