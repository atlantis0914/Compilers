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
    sortedSizes = sortBy (comparing snd) $ map (\(typ,s) -> (s, getSizeForType tDefs sDefs typ)) fields
    -- We could scan here, but the 0 mod 4 vs 0 mod 8 shit is nasty 
    (fieldOffsets, totalSize) = genFieldOffsets sDefs sortedSizes Map.empty 0
    sortedAlignments = sortBy (comparing snd) $ map (\s -> (s, getStrictFieldSize tDefs sDefs s)) (map fst fields)
    alignment = snd $ last sortedAlignments 
  in
    SDefn name fields fieldOffsets alignment totalSize  p

genFieldOffsets :: StructDefs -> [(String, Int)] -> StructOffsets 
                                   -> Int -> (StructOffsets, Int)
genFieldOffsets sDefs [] m i = (m, i)
genFieldOffsets sDefs ((name,size):xs) m i = 
  -- We either have a 'primitive' type, a * or [], or a concrete struct. 
  case (size, i `mod` 8) of
    (4, _) -> genFieldOffsets sDefs xs (Map.insert name (size, i) m) (i + 4)
    (8, 0) -> genFieldOffsets sDefs xs (Map.insert name (size, i) m) (i + 8)
    (8, 4) -> genFieldOffsets sDefs xs (Map.insert name (size, i+4) m) (i + 12)
    (_, _) -> genFieldOffsets sDefs xs (Map.insert name (size, i') m) (i'')
  where
    (i',i'') = genForStruct name size (i `mod` 8) i
  
    genForStruct :: String -> Int -> Int -> Int -> (Int, Int)
    genForStruct name size imod8 i = 
      case (structAlign name, imod8) of 
        (_, 0) -> (i, i + size)
        (4, _) -> (i, i + size)
        (8, 4) -> (i+4, i + size + 4) -- 4 bytes of padding
      
    structAlign name = 
      case (sDefs Map.! name) of
        Nothing -> error ("Stcut " ++ name ++ " must be decled")
        Just (SDefn {structAlignment = align}) -> align

    

checkStructFields :: SDefn -> SDefn 
checkStructFields s@(SDefn name fields _ _ _ _) = 
  let
    names = map fst fields 
    uniques = nub names 
  in
    if (length uniques == length names) 
      then s
      else error ("Struct defn : " ++ name ++ " has non-unique names")

checkStructDecl :: PGDecl -> StructDefs -> StructDefs
checkStructDecl (PSDecl (ParseSDecl name _) _) sMap = 
  Map.insert name Nothing sMap

checkStructDefn :: PGDecl -> StructDefs -> StructDefs
checkStructDefn (PSDefn sdefn@(ParseSDefn name fields _) _) sMap = 
  -- We just want to generate the name - the struct cannot be referred
  -- to explicitly yet. 
  Map.insert name Nothing sMap



