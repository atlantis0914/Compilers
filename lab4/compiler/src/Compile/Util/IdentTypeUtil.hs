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

isStruct :: IdentType -> Bool
isStruct (IStruct (ITypeDef _)) = True
isStruct _ = False

getSizeForType :: Map.Map String (Maybe SDefn) -> IdentType -> Int
getSizeForType m IInt = 4
getSizeForType m IBool = 4
getSizeForType m IVoid = error ("Trying to get size for void")
getSizeForType m (IPtr _) = 8
getSizeForType m (IArray _) = 8
getSizeForType m (IStruct name) = 
  case (Map.lookup name m) of
    Nothing -> error ("Struct " ++ name ++ " must be declared before use")
    Just (SDefn {structSize = size}) = size
