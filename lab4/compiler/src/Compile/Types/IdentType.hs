module Compile.Types.IdentType where

import qualified Data.Map as Map

data IdentType = IInt
               | IBool 
               | IVoid 
               | IPtr IdentType
               | IArray IdentType
               | IStruct IdentType
               | ITypeDef String deriving (Eq, Show, Ord)

getIdentTypeMap = Map.fromList $ [("int", IInt), 
                                  ("bool", IBool),
                                  ("void", IVoid)]


