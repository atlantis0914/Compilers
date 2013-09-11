module Compile.Util.Graph where 

import Compile.Types
import Data.Map

isEdge :: (Ord a) => (Graph a) -> a -> a -> Bool
isEdge (Graph m) src target = 
  case (Data.Map.lookup src m) of
    Nothing -> False
    Just srcV -> case (Data.Map.lookup target (vertexAdjacencies srcV)) of
                   Nothing -> False
                   Just _ -> True

newGraph :: (Graph a)
newGraph = Graph (empty)

newVertex :: a -> Vertex a
newVertex s = Vertex {vertexData = s, vertexAdjacencies = empty}

addVertexSafe :: (Ord a) => (Graph a) -> a -> (Graph a, Vertex a)
addVertexSafe (Graph m) s = 
  case (Data.Map.lookup s m) of
    Just v -> (Graph m, v)
    Nothing -> let v = newVertex s in (Graph (insert s (newVertex s) m), v)

addEdgeSafe :: (Ord a) => (Graph a) -> a -> a  -> (Graph a)
addEdgeSafe (Graph m) src target = 
  let
    (Graph m', srcV) = addVertexSafe (Graph m) src
    adjacencies = vertexAdjacencies srcV
    adjacencies' = insert target () adjacencies
    srcV' = Vertex {vertexData = src, vertexAdjacencies = adjacencies'}
  in
    Graph (insert src srcV' m')
