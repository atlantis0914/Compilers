module Compile.Types.Color where 

import qualified Data.Map as Map
import Compile.Types.AbstractAssembly

data Color = Uncolored 
             Color Int deriving (Eq, Show)

type ColoringMap = Map.Map ALoc Color
