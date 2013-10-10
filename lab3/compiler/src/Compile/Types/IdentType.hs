module Compile.Types.IdentType where

import qualified Data.Map as Map

data IdentType = IInt
               | IBool 
               | IVoid 
               | ITypeDef String deriving (Eq, Show)

getIdentTypeMap = Map.fromList $ [("int", IInt), 
                                  ("bool", IBool),
                                  ("void", IVoid)]
