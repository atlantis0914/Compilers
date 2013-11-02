module Compile.Types.IdentType where

import qualified Data.Map as Map
import Control.DeepSeq

data IdentType = IAny
               | IInt
               | IBool
               | IVoid
               | IPtr IdentType
               | IArray IdentType
               | IStruct IdentType
               | ITypeDef String deriving (Eq, Ord)

getIdentTypeMap = Map.fromList $ [("int", IInt),
                                  ("bool", IBool),
                                  ("void", IVoid)]

getIdentSize IInt = 4
getIdentSize IBool = 4
getIdentSize (IPtr _) = 8

instance Show IdentType where
  show IAny = "any"
  show IInt = "int"
  show IBool = "bool"
  show IVoid = "void"
  show (IPtr i) = "" -- "(" ++ show i ++ ")" ++ "*"
  show (IArray i) = "" -- show i ++ "[]"
  show (IStruct i) = "struct (" ++ show i ++ ")"
  show (ITypeDef s) = s

instance NFData IdentType where
  rnf IAny = ()
  rnf IInt = ()
  rnf IBool = ()
  rnf IVoid = ()
  rnf (IPtr i) = i `deepseq` ()
  rnf (IArray i) = i `deepseq` () 
  rnf (IStruct i) = i `deepseq` ()
  rnf (ITypeDef s) = s `deepseq` ()
