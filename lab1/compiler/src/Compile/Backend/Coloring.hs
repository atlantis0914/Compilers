module Compile.Backend.Coloring where

import Compile.Types
import Compile.Util.Graph
import Compile.Util.Color

import Data.List
import qualified Data.Map as Map

-- Takes an Interference Graph, and the Simplicial Elimination Ordering
-- and produces a coloring of the graph 
greedyColor :: Graph ALoc -> [ALoc] -> ColoringMap
greedyColor g ordering = foldl (colorStep g) (instantiateColoringMap ordering) ordering

-- Used to foldr an ALoc into a coloring for that set of locs. 
colorStep :: Graph ALoc -> ColoringMap -> ALoc -> ColoringMap
colorStep (Graph graphMap) colorMap aloc = 
  let
    Vertex {vertexAdjacencies = nghMap} = graphMap Map.! aloc
    neighborsLocs = Map.keys nghMap
    neighborColors = filter (isColor)
                            (map (\a -> colorMap Map.! a) neighborsLocs)
    color = getLowestColor neighborColors
  in
    Map.insert aloc color colorMap

-- This is implemented really slowly for the time being. This should 
-- be fixed as time permits
getLowestColor :: [Color] -> Color 
getLowestColor colorList = findLowest (sort colorList)

findLowest :: [Color] -> Color
findLowest l = findLowest' l (Color 0) 

findLowest' :: [Color] -> Color -> Color
findLowest' [] c = c
findLowest' [x] c@(Color i) = if (x == c) then Color (i + 1) else c
findLowest' (x:xs) c@(Color i) = if (x == c) then findLowest' xs (Color (i+1)) else c
