module Compile.Types.IdentType where

data IdentType = IInt
               | IBool 
               | IVoid 
               | ITypeDef String deriving (Eq, Show)

