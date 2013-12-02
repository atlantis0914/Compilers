module Compile.Asm.StructAsm where 

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

import Compile.Asm.AsmTypes

import Control.Monad.State

genStructMap :: [IRDecl] -> StructMap
genStructMap decls = genStructMap' (Map.empty) decls
  where 
    genStructMap' :: StructMap -> [IRDecl] -> StructMap
    genStructMap' m [] = m
    genStructMap' m ((IRSDefn (strct@(IRStructDef {strctName = name}))):xs) = 
      genStructMap' (Map.insert name strct m) xs
    genStructMap' m (x:xs) = genStructMap' m xs

genStructAccessorsFromMap :: StructMap -> String
genStructAccessorsFromMap m = concatMap (\x -> genStructAccessors x) (Map.elems m)

genStructAccessors :: IRStructDef -> String
genStructAccessors (IRStructDef name fields types offs algn size) = 
  let
    accessors = concatMap (genStructAccessor name) (Map.toList offs)
  in
    accessors

genStructAccessor :: String -> (String, (Int,Int)) -> String
genStructAccessor pref (fieldName ,(sz, off)) = 
  "  function " ++ pref ++ "_" ++ fieldName ++ " (base) {" ++ "\n" ++ 
  "    base = base | 0;" ++ "\n" ++ 
  "    return H32[base + " ++ (show (off `div` 4)) ++ " >> 2] | 0;" ++ "\n" ++ 
  "  }\n" 
