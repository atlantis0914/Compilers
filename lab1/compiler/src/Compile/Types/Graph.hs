module Compile.Types.Graph where 

import Data.Map
import Compile.Types.Color

data Vertex a = Vertex {vertexData :: a
                       ,vertexAdjacencies :: Map a ()
                       ,vertexCardinality :: Int
                       ,vertexIsLive :: Bool 
                       ,vertexColor :: Color
                       ,prohibitedColors :: [Color] 
                       } deriving (Eq, Show)

data Graph a = Graph (Map a (Vertex a)) deriving (Eq, Show)
