module Compile.Backend.MaximumCardinalitySearch where 

import Compile.Types
import Data.List 
import Data.Map

import Compile.Util.Graph

import Debug.Trace

-- Takes an interference graph and outputs a simplicial elimination ordering
maximumCardinalitySearch :: Graph ALoc -> [ALoc]
maximumCardinalitySearch (Graph g) = mcs (Graph g) (length (elems g)) []

mcs :: Graph ALoc -> Int -> [ALoc] -> [ALoc]
mcs (Graph g) n l = 
  if (length l == n)
  then reverse l 
  else 
    let
      (g', Vertex {vertexData = v}) = chooseVertex (Graph g)
    in 
      mcs g' n (v:l)

chooseVertex :: Graph ALoc -> (Graph ALoc, Vertex ALoc)
chooseVertex g = 
  let
    (g', Just v) = chooseVertex' g
  in
    (g', v)

chooseVertex' g@(Graph gMap) = 
  let 
    v = elems gMap
    Just (max) = 
          Data.List.foldr (\v@(Vertex {vertexIsLive = isLive}) -> 
                 \curMax -> 
                    if (not isLive)
                      then curMax
                      else case curMax of 
                          Nothing -> Just v
                          Just v' -> Just $ maxCardinality v v')
                 (Nothing) (v)
   in
     updateGraphWithVert g max
    where updateGraphWithVert (Graph g) (vert@(Vertex {vertexData = v})) = 
            let
              g' = setVertexLiveTo (Graph g) v False
              g'' = incrementOutCardinalities g' v 
            in
             (g'', Just vert) 

maxCardinality :: Vertex ALoc -> Vertex ALoc -> Vertex ALoc
maxCardinality (v1@(Vertex {vertexCardinality = v1Card})) 
               (v2@(Vertex {vertexCardinality = v2Card})) = 
  case (v1Card `compare` v2Card) of
    LT -> v2
    otherwise -> v1
