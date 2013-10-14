module Compile.Frontend.ElaborateType where 

import Compile.Types
import qualified Data.Map as Map

import qualified Debug.Trace as Trace

type TypeDefs = Map.Map IdentType IdentType

checkTDIdent :: TypeDefs -> String -> String
checkTDIdent typeDefs s = 
  -- Have to force s to ITypeDef to do the lookup
  Trace.trace ("tdef : " ++ show typeDefs ++ " i " ++ s) $ case (Map.lookup (ITypeDef s) typeDefs) of 
    Just _ -> error ("Ident " ++ s ++ " has conflicting type-def")
    Nothing -> s

checkTDIdentList :: TypeDefs -> [String] -> [String]
checkTDIdentList typeDefs l = 
  if (ok)
    then l
    else error ("Conflicting type def")
  where 
    ok = all (\s -> (not $ Map.member (ITypeDef s) typeDefs)) l

elaborateTDIdentType :: TypeDefs -> IdentType -> IdentType
elaborateTDIdentType typeDefs t = 
  case (Map.lookup t typeDefs) of 
    Just simp -> simp
    Nothing -> error ("Used unknown type : " ++ show t)

elaborateTDIdentTypes :: TypeDefs -> [IdentType] -> [IdentType]
elaborateTDIdentTypes typeDefs l = map (elaborateTDIdentType typeDefs) l
