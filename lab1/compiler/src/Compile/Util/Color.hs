module Compile.Util.Color where 

import Compile.Types
import qualified Data.Map as Map
import Data.List

-- Creates a new ColoringMap where each location points to Uncolored
instantiateColoringMap :: [ALoc] -> ColoringMap
instantiateColoringMap alocs =
  foldl (\m -> \a -> (Map.insert a Uncolored m)) (Map.empty) alocs

isColor :: Color -> Bool
isColor (Color i) = True
isColor Uncolored = False
