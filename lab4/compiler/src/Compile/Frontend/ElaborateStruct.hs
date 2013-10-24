module Compile.Frontend.ElaborateStruct where 

import Compile.Types
import Data.List
import qualified Data.Map as Map

import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

-- Nothing, if it's only been decl'd. Otherwise Just (Definition)
type StructDefs = Map.Map String (Maybe SDefn)

-- A map from the fieldName -> (fieldSize, fieldOffset)
type StructOffsets = Map.Map String (Int, Int)

generateStructDefn :: ParseSDefn -> StructDefs -> SDefn
generateStructDefn (ParseSDefn name fields p) sDefs = 
  let
    sortedSizes = sortBy (compare snd) $ map (\s -> (s, getSizeForType m s)) (map snd fields)
    -- We could scan here, but the 0 mod 4 vs 0 mod 8 shit is nasty 
    (fieldOffsets, totalSize) = generateFieldOffsets sDefs fields Map.empty 0
  in
    SDefn name fields fieldOffsets structSize p

genFieldOffsets :: StructDefs -> [(String, Int)] -> StructOffsets 
                                   -> Int -> (StructOffsets, Int)
genFieldOffsets sDefs [] m i = (m, i)
genFieldOffsets sDefs ((name,size):xs) m i = 
  -- We either have a 'primitive' type, a * or [], or a concrete struct. 
  case (size, i `mod` 8) of
    (4, _) -> genFieldOffsets xs (Map.insert name (size, i)) (i + 4)
    (8, 0) -> genFieldOffsets xs (Map.insert name (size, i)) (i + 8)
    (8, 4) -> genFieldOffsets xs (Map.insert name (size, i+4)) (i + 12)
    (_, _) -> genFieldOffsets xs (Map.insert name (size, i')) (i'')
  where
    (i',i'') = genForStruct name size (i `mod` 8) i
    align = structAlign name  
    genForStruct name size mod8 i = 
      case (align, mod8) of 
        (_, 0) -> (i, i + size)
        (4, _) -> (i, i + size)
        (8, 4) -> (i+4, i + size + 4) -- 4 bytes of padding
      
    structAlign name = 
      case (sDefs Map.! name) of
        Nothing -> error ("Stcut " ++ name ++ " must be decled")
        Just (SDefn {structAlignment = align}) -> align
    

checkStructFields :: SDefn -> SDefn 
checkStructFields s@(SDefn name fields p) = 
  let
    names = map fst fields 
    uniques = nub names 
  if (length uniques == length names) 
    then s
    else error ("Struct defn : " ++ name " has non-unique names")

checkStructDecl :: PGDecl -> StructDefs -> StructDefs
checkStructDecl (PSDecl (ParseSDecl name _)) sMap = 
  Map.insert name Nothing sMap

checkStructDefn :: PGDecl -> StructDefs -> StructDefs
checkStructDefn (PSDefn sdefn@(ParseSDefn name fields _)) sMap = 
  Map.insert name (Just sdefn) sMap
