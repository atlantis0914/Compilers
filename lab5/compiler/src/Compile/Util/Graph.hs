module Compile.Util.Graph where 

import Compile.Types
import Data.Map
import qualified Data.List as List
import qualified Data.Set as Set

import qualified Debug.Trace as Trace

-- Determines whether or not the directed edge (src, target) is in the graph
isEdge :: (Ord a) => (Graph a) -> a -> a -> Bool
isEdge (Graph m) src target = 
  case (Data.Map.lookup src m) of
    Nothing -> False
    Just srcV -> case (Data.Map.lookup target (vertexAdjacencies srcV)) of
                   Nothing -> False
                   Just _ -> True

getNeighborsAsSet :: (Ord a) => Graph a -> a -> Set.Set a
getNeighborsAsSet (Graph m) src = 
  case (Data.Map.lookup src m) of 
    Nothing -> Set.empty
    Just srcV -> keysSet (vertexAdjacencies srcV) 

updateVertexPtrs :: (Ord a) => Graph a -> a -> a -> Graph a
updateVertexPtrs (Graph m) old new = 
  Graph $ Data.Map.map (\(v@(Vertex {vertexAdjacencies = adj})) ->   
            if (Data.Map.member old adj) 
              then v {vertexAdjacencies = (Data.Map.insert new () (Data.Map.delete old adj))}
              else v) m

-- Creates a new Graph 
newGraph :: (Graph a)
newGraph = Graph (empty)

-- Creates a new vertex, with 0 cardinality
newVertex :: a -> Vertex a
newVertex s = Vertex {vertexData = s, 
                      vertexAdjacencies = empty, 
                      vertexCardinality = 0, 
                      vertexIsLive = True,
                      vertexColor = Uncolored,
                      prohibitedColors = []}

extractColor :: (Ord a) => (Graph a) -> a -> Color 
extractColor (Graph m) s =
  case (Data.Map.lookup s m) of 
    Nothing -> error ("Bad vertex looked up in extractColor")
    Just (Vertex {vertexColor = color}) -> color 

-- Safely adds a vertex to a graph
addVertexSafe :: (Ord a) => (Graph a) -> a -> (Graph a, Vertex a)
addVertexSafe (Graph m) s = 
  case (Data.Map.lookup s m) of
    Just v -> (Graph m, v)
    Nothing -> let v = newVertex s in (Graph (insert s (newVertex s) m), v)

addVertexGetGraph :: (Ord a) => a -> (Graph a) -> (Graph a)
addVertexGetGraph s g = g'
  where (g',_) = addVertexSafe g s

-- Safely adds the edge (src, target) to the graph 
addEdgeSafe :: (Graph ALoc) -> ALoc -> ALoc  -> (Graph ALoc)

addEdgeSafe g@(Graph m) (AReg _ _) (_) = g

addEdgeSafe g@(Graph m) src@(ATemp tempNum _) reg@(AReg regNum b) = 
  let 
    (Graph m', srcV@(Vertex {prohibitedColors = colorList})) = addVertexSafe (g) src 
    srcV' = srcV {prohibitedColors = ((Color regNum):colorList) }
  in
    (Graph $ insert src srcV' m')

addEdgeSafe (Graph m) src target = 
  let
    (Graph m', srcV) = addVertexSafe (Graph m) src
    adjacencies = vertexAdjacencies srcV
    adjacencies' = insert target () adjacencies
    srcV' = srcV {vertexAdjacencies = adjacencies'}
  in
    Graph (insert src srcV' m')

addEdgeFoldPat :: ALoc -> (Graph ALoc) -> ALoc -> (Graph ALoc)
addEdgeFoldPat l1 g l2 = addEdgeSafe g l1 l2

-- Sets this vertex's live value to the specified boolean
setVertexLiveTo :: (Ord a) => Graph a -> a -> Bool -> Graph a
setVertexLiveTo (Graph g) v b = 
  let
    vert = g ! v 
    vert' = vert {vertexIsLive = b}
  in
    Graph (insert v vert' g)

-- Increments the vertex's cardinality by one
incrementOutCardinality :: (Ord a) => a -> Graph a -> Graph a
incrementOutCardinality v (Graph g) = 
  let
    vert@(Vertex {vertexCardinality = card}) = g ! v
    vert' = vert {vertexCardinality = (card + 1)}
  in
    Graph (insert v vert' g)

-- Increments all out neighbours cardinalities by one
incrementOutCardinalities :: (Ord a) => Graph a -> a -> Graph a
incrementOutCardinalities (Graph g) v = 
  let 
    outList = keys $ vertexAdjacencies $ g ! v
  in
    List.foldr incrementOutCardinality (Graph g) outList
