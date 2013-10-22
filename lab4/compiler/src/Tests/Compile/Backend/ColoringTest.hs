module Tests.Compile.Backend.ColoringTest (coloringTest) where 

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.Coloring

import Compile.Util.Graph

import qualified Data.Map as Map

-- greedyColor :: Graph ALoc -> [ALoc] -> ColoringMap

v1@(Vertex {vertexAdjacencies = m1}) = newVertex (ATemp 1)
v1' = v1 {vertexAdjacencies = (Map.insert (ATemp 2) () m1)}

v2@(Vertex {vertexAdjacencies = m2}) = newVertex (ATemp 2)
v2' = v2 {vertexAdjacencies = Map.insert (ATemp 3) () (Map.insert (ATemp 1) () m2) }

v3@(Vertex {vertexAdjacencies = m3}) = newVertex (ATemp 3)
v3' = v3 {vertexAdjacencies = Map.insert (ATemp 2) () (Map.insert (ATemp 4) () m3) }

v4@(Vertex {vertexAdjacencies = m4}) = newVertex (ATemp 4)
v4' = v4 {vertexAdjacencies = (Map.insert (ATemp 3) () m4)}

v5' = newVertex (ATemp 5)

v6' = newVertex (ATemp 6)

g1newGraph@(Graph g1Map) = newGraph
newMap = (Map.insert (ATemp 1) v1' (Map.insert (ATemp 2) v2' (Map.insert (ATemp 3) v3'
         (Map.insert (ATemp 4) v4' (Map.insert (ATemp 5) v5' 
         (Map.insert (ATemp 6) v6' g1Map))))))
g1 = Graph newMap

alloc1 = [v1', v2', v3', v4', v5', v6']

coloring1 = greedyColor g1 alloc1

expected1 = (Map.insert (ATemp 1) (Color 0) (Map.insert (ATemp 2) (Color 1)
            (Map.insert (ATemp 3) (Color 0) (Map.insert (ATemp 4) (Color 1) 
            (Map.insert (ATemp 5) (Color 0) (Map.insert (ATemp 6) (Color 0) Map.empty))))))

test1 :: Assertion
test1 = assertEqual "graphColor"
        expected1
        coloring1

colors2 = [Color 0, Color 2, Color 3, Color 4]
test2 :: Assertion
test2 = assertEqual "getLowestColorTest"
        (Color 1)
        (getLowestColor colors2)

colors3 = [Color 2, Color 3, Color 4, Color 0, Color 1]
test3 :: Assertion
test3 = assertEqual "getLowestColorTest"
        (Color 5)
        (getLowestColor colors3)

colors4 = [Uncolored, Uncolored, Uncolored, Uncolored]
test4 :: Assertion
test4 = assertEqual "getLowestColorTest"
        (Color 0)
        (getLowestColor colors4)

colors5 = [Color 1, Color 2]
test5 :: Assertion
test5 = assertEqual "getLowestColorTest"
        (Color 0)
        (getLowestColor colors5)

coloringTest = [testCase "graphColor-1" test1,
                testCase "lowestColor-1" test2,
                testCase "lowestColor-2" test3,
                testCase "lowestColor-3" test4,
                testCase "lowestColor-4" test5
               ]
