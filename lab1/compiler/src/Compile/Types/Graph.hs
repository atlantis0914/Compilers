module Compile.Types.Graph where 

import Data.Map

data Vertex a = Vertex {vertexData :: a
                       ,vertexAdjacencies :: Map a ()
                       ,vertexCardinality :: Int
                       ,vertexIsLive :: Bool 
                       } deriving (Eq, Show)

data Graph a = Graph (Map a (Vertex a)) deriving (Eq, Show)
