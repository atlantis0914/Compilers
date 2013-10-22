module Compile.Types.Color where

import qualified Data.Map as Map
import Compile.Types.AbstractAssembly

data Color = Uncolored
           | Color Int deriving (Eq, Show)

type ColoringMap = Map.Map ALoc Color

instance Ord Color where
  Uncolored `compare` (Color i) = LT
  (Color i) `compare` Uncolored = GT
  (Color i) `compare` (Color i') = i `compare` i'
  Uncolored `compare` Uncolored = EQ
