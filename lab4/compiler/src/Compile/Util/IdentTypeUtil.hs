module Compile.Util.IdentTypeUtil where 

import Compile.Types

import qualified Data.Map as Map
import qualified Debug.Trace as Trace

type TypeDefs = Map.Map IdentType IdentType

toIdentType :: String -> IdentType
toIdentType "int" = IInt
toIdentType "bool" = IBool
toIdentType "void" = IVoid
toIdentType s = ITypeDef s

tM = [(IAny, IAny),
      (IInt, IInt),
      (IBool, IBool),
      (IVoid, IVoid)]

baseIdentTypeMap = Map.fromList tM

isStruct :: IdentType -> Bool
isStruct (IStruct (ITypeDef _)) = True
isStruct _ = False

isSmallType :: IdentType -> Bool
-- isSmallType IAny = True -- shouldn't need this 
isSmallType IInt = True
isSmallType IBool = True
isSmallType (IPtr _) = True
isSmallType (IArray _) = True
isSmallType IVoid = True
isSmallType (IStruct _) = error ("Struct type not small")
isSmallType (ITypeDef _) = error ("Why do you have typedefs when calling isSmallType bro")

-- Unwraps ptrs and arrays to get at a 'concrete' type. Note
-- that this also includes STRUCTS, as they add to the type-space
-- in a concrete, non-trivial way. 
simplifyTypeDefdType :: TypeDefs -> IdentType -> IdentType 
simplifyTypeDefdType td (IPtr i) = IPtr (simplifyTypeDefdType td i)
simplifyTypeDefdType td (IArray i) = IArray (simplifyTypeDefdType td i) 
-- This is the special case to ensure that we don't throw check-errors for 
-- structs that have not been explicitly declared. We don't enforce that a struct
-- must be declared until type-checking when we see a USE of the struct's fields. 
simplifyTypeDefdType td s@(IStruct (ITypeDef name)) = 
  case (Map.lookup s td) of 
    Just simp -> simp
    Nothing -> s
simplifyTypeDefdType td s = 
  case (Map.lookup s td) of 
    Just simp -> simp
    Nothing -> error ("Used unknown type :: " ++ show s)

-- Checks to see if the type is a valid concrete type
isValidConcreteType :: TypeDefs -> Map.Map String SDefn -> IdentType -> Bool
isValidConcreteType td sm IInt = True
isValidConcreteType td sm IBool = True
isValidConcreteType td sm IVoid = False
isValidConcreteType td sm (IPtr (IStruct (ITypeDef _))) = True
isValidConcreteType td sm (IArray (IStruct (ITypeDef _))) = True
isValidConcreteType td sm (IPtr i) = isValidConcreteType td sm i
isValidConcreteType td sm (IArray i) = isValidConcreteType td sm i
isValidConcreteType td sm s@(IStruct (ITypeDef name)) = False
--  case (Map.lookup name sm) of 
--    Just simp -> True
--    Nothing -> False
isValidConcreteType td sm s = 
  case (Map.lookup s td) of 
    Just simp -> isValidConcreteType td sm simp
    Nothing -> False

concreteTypeExists :: TypeDefs -> Map.Map String SDefn -> IdentType -> Bool
concreteTypeExists td sm IInt = True
concreteTypeExists td sm IBool = True
concreteTypeExists td sm IVoid = False
concreteTypeExists td sm (IPtr (IStruct (ITypeDef _))) = True
concreteTypeExists td sm (IArray (IStruct (ITypeDef _))) = True
concreteTypeExists td sm (IPtr i) = concreteTypeExists td sm i
concreteTypeExists td sm (IArray i) = concreteTypeExists td sm i
concreteTypeExists td sm s@(IStruct (ITypeDef name)) = 
  case (Map.lookup name sm) of 
    Just simp -> True
    Nothing -> False
concreteTypeExists td sm s = 
  case (Map.lookup s td) of 
    Just simp -> concreteTypeExists td sm simp
    Nothing -> False



getSizeForType :: TypeDefs -> Map.Map String (Maybe SDefn) -> IdentType -> Int
getSizeForType _ _ IInt = 4
getSizeForType _ _ IBool = 4
getSizeForType _ _ IVoid = error ("Trying to get size for void")
getSizeForType _ _ (IPtr _) = 8
getSizeForType _ _ (IArray _) = 8
getSizeForType _ m (IStruct (ITypeDef name)) = 
  case (Map.lookup name m) of
    Just (Just (SDefn {structSize = size})) -> size
    _ -> error ("Struct " ++ name ++ " must be declared before use")
getSizeForType t m (ITypeDef name) = 
  case (Map.lookup (toIdentType name) t) of 
    Nothing -> error ("TypeName " ++ name ++ " not found in context")
    Just (typ) -> getSizeForType t m typ

getStrictFieldSize :: TypeDefs -> Map.Map String (Maybe SDefn) -> IdentType -> Int 
getStrictFieldSize _ _ IInt = 4
getStrictFieldSize _ _ IBool = 4
getStrictFieldSize _ _ IVoid = error ("No size for void")
getStrictFieldSize _ _ (IPtr _) = 8
getStrictFieldSize _ _(IArray _) = 8
getStrictFieldSize _ m (IStruct (ITypeDef name)) = 
  case (Map.lookup name m) of 
    Just (Just (SDefn {structAlignment = align})) -> align
    _ -> error ("Struct " ++ name ++ " must be declared before use")
getStrictFieldSize t m (ITypeDef name) = 
  case (Map.lookup (toIdentType name) t) of 
    Nothing -> error ("TypeName " ++ name ++ " not found in context")
    Just (typ) -> getStrictFieldSize t m typ
-- should probably path-compress the shit out of this, but that's overkill yo

getIDLVal :: LValue -> String 
getIDLVal (LExpr e _) = getIDExpr e

-- Only need to define over mem exprs 
getIDExpr (ExpBinMem o e1 e2 _) = getIDExpr e1
getIDExpr (ExpUnMem o e1 _) = getIDExpr e1
getIDExpr (Ident s _) = s
