module Compile.Frontend.ElaborateType where 

import Compile.Types
import qualified Data.Map as Map

type TypeDefs = Map.Map IdentType IdentType

checkTDIdent :: TypeDefs -> String -> String
checkTDIdent typeDefs s = 
  -- Have to force s to ITypeDef to do the lookup
  case (Map.lookup (ITypeDef s) typeDefs) of 
    Just _ -> error ("Ident " ++ s ++ " has conflicting type-def")
    Nothing -> s

checkTDIdentList :: TypeDefs -> [String] -> [String]
checkTDIdentList typeDefs l = map (checkTDIdent typeDefs) l

elaborateTDIdentType :: TypeDefs -> IdentType -> IdentType
elaborateTDIdentType typeDefs t = 
  case (Map.lookup t typeDefs) of 
    Just simp -> simp
    Nothing -> error ("Used unknown type : " ++ show t)

elaborateTDIdentTypes :: TypeDefs -> [IdentType] -> [IdentType]
elaborateTDIdentTypes typeDefs l = map (elaborateTDIdentType typeDefs) l
