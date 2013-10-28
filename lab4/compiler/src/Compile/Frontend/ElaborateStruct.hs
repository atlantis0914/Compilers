module Compile.Frontend.ElaborateStruct where 

import Compile.Types
import Data.List
import Data.Ord
import qualified Data.Map as Map

import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil
import Compile.Frontend.ElaborateType

-- Nothing, if it's only been decl'd. Otherwise Just (Definition)
type StructDefs = Map.Map String (Maybe SDefn)

-- A map from the fieldName -> (fieldSize, fieldOffset)
-- Imported from PostElabAST
-- type StructOffsets = Map.Map String (Int, Int)

generateStructDefn :: TypeDefs -> StructDefs -> ParseSDefn  -> SDefn
generateStructDefn tDefs sDefs (ParseSDefn name fields p)  = 
  let
    sortedSizes = sortBy (comparing snd) $ map (\(typ,s) -> ((typ,s), getSizeForType tDefs sDefs typ)) fields
    -- We could scan here, but the 0 mod 4 vs 0 mod 8 shit is nasty 
    (fieldOffsets, totalSize) = genFieldOffsets sDefs sortedSizes Map.empty 0
    sortedAlignments = sortBy (comparing snd) $ map (\s -> (s, getStrictFieldSize tDefs sDefs s)) (map fst fields)
    alignment = if (length sortedAlignments == 0)
                  then 0
                  else snd $ last sortedAlignments 
    structTypeMap = Map.fromList (map (\(t,s) -> (s,t)) fields)
  in
    SDefn name fields structTypeMap fieldOffsets alignment totalSize  p

genFieldOffsets :: StructDefs -> [((IdentType, String), Int)] -> StructOffsets 
                                   -> Int -> (StructOffsets, Int)
genFieldOffsets sDefs [] m i = (m, i)
genFieldOffsets sDefs (((typ,name),size):xs) m i = 
  -- We either have a 'primitive' type, a * or [], or a concrete struct. 
  case (size, i `mod` 8) of
    (4, _) -> genFieldOffsets sDefs xs (Map.insert name (size, i) m) (i + 4)
    (8, k) -> genFieldOffsets sDefs xs (Map.insert name (size, i + k) m) (i + 8 + k)
    (_, _) -> genFieldOffsets sDefs xs (Map.insert name (size, i') m) (i'')
  where
    (i',i'') = genForStruct typ name size (i `mod` 8) i
  
    genForStruct :: IdentType -> String -> Int -> Int -> Int -> (Int, Int)
    genForStruct (IStruct (ITypeDef structName)) name size imod8 i = 
      case (structAlign structName, imod8) of 
        (_, 0) -> (i, i + size)
        (4, _) -> (i, i + size)
        (8, 4) -> (i+4, i + size + 4) -- 4 bytes of padding
      
    structAlign name = 
      case (sDefs Map.! name) of
        Nothing -> error ("Struct " ++ name ++ " must be declared before concrete use")
        Just (SDefn {structAlignment = align}) -> align
    

checkStructFields :: StructDefs -> SDefn -> SDefn 
checkStructFields sMap s@(SDefn name fields _ _ _ _ _) = 
  let
    names = map snd fields 
    uniques = nub names 
  in
    if (length uniques == length names) 
      then checkLargeStructFields sMap s
      else error ("Struct defn : " ++ name ++ " has non-unique names")

checkLargeStructFields :: StructDefs -> SDefn -> SDefn 
checkLargeStructFields sMap s@(SDefn name fields _ _ _ _ _) = 
  let
    fields' = map (checkFieldType sMap) fields
  in
    s {structFields = fields'}

checkFieldType :: StructDefs -> (IdentType, String) -> (IdentType, String)
checkFieldType sMap (typ, name) = 
  case (typ) of 
    (IStruct (ITypeDef sName)) -> 
      if (Map.member sName sMap)  
        then Trace.trace ("name : " ++ name ++ " is member") $ (typ, name)
        else error ("Using concrete name of : " ++ name ++ " in struct defn before define")
    _ -> Trace.trace (" name is not struct" ++ name) $ (typ, name)


-- Called when parsing a Decl - this isn't a declaration so we just insert 
-- Nothing into the ctx. 
checkStructDecl :: PGDecl -> StructDefs -> StructDefs
checkStructDecl (PSDecl (ParseSDecl name _) _) sMap = sMap
--   case (Map.lookup name sMap) of
--     Nothing -> Map.insert name Nothing sMap
--     _ -> sMap

-- Called initially, when the pre-elab struct is being processed. Only inserts
-- a Nothing into the ctx. 
checkStructDefn :: PGDecl -> StructDefs -> StructDefs
checkStructDefn (PSDefn sdefn@(ParseSDefn name fields p) _) sMap = 
  case (Map.lookup name sMap) of 
    Just (Just _) -> error ("Struct " ++ name ++ " multiple declared at " ++ show p)
    _ -> Map.insert name Nothing sMap

-- Called after the entire struct has been processed. Adds the concrete 
-- struct definition to the map. 
addStructDefn :: GDecl -> StructDefs -> StructDefs
addStructDefn (GSDefn sdefn@(SDefn {structName = name}) _) sDefs = 
  Map.insert name (Just sdefn) sDefs 
