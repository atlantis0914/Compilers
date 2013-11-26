module Tests.Compile.Backend.InterferenceTest (interferenceTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.Interference
import Compile.Util.Graph

import Data.List
import qualified Data.Map  as Map

t1aasm1 = AAsm {aAssign = [ATemp 0], aOp = Nop, aArgs = [AImm 2]}
t1aasm2 = AAsm {aAssign = [ATemp 1], aOp = Add, aArgs = [ALoc(ATemp 0), AImm 4]}
t1aasm3 = AAsm {aAssign = [ATemp 2], aOp = Nop, aArgs = [ALoc(ATemp 0)]}

t1aasmList = [t1aasm1, t1aasm2, t1aasm3]

t1alocsList  = [[]
          ,[ATemp 0]
          ,[ATemp 0]]

test1 :: Assertion
test1 = assertEqual "testInterferenceEdges"
                    (sort $ [Edge(ATemp 0, ATemp 1), Edge(ATemp 1, ATemp 0)])
                    (sort $ getInterferenceEdges t1aasmList t1alocsList)

t2aasmList =
  [AAsm {aAssign = [ATemp 1], aOp = Nop, aArgs = [AImm 1]}
  ,AAsm {aAssign = [ATemp 2], aOp = Nop, aArgs = [AImm 1]}
  ,AAsm {aAssign = [ATemp 3], aOp = Add, aArgs = [ALoc(ATemp 2), ALoc(ATemp 1)]}
  ,AAsm {aAssign = [ATemp 4], aOp = Add, aArgs = [ALoc(ATemp 3), ALoc(ATemp 2)]}
  ,AAsm {aAssign = [ATemp 5], aOp = Add, aArgs = [ALoc(ATemp 4), ALoc(ATemp 3)]}
  ,AAsm {aAssign = [ATemp 6], aOp = Nop, aArgs = [ALoc(ATemp 5)]}
  ]

t2alocsList =
  [[]
  ,[ATemp 1]
  ,[ATemp 2, ATemp 1]
  ,[ATemp 3, ATemp 2]
  ,[ATemp 3, ATemp 4]
  ,[ATemp 5]
  ]

t2Res =
  [Edge(ATemp 1, ATemp 2)
  ,Edge(ATemp 2, ATemp 1)
  ,Edge(ATemp 2, ATemp 3)
  ,Edge(ATemp 3, ATemp 2)
  ,Edge(ATemp 3, ATemp 4)
  ,Edge(ATemp 4, ATemp 3)
  ]

test2 :: Assertion
test2 = assertEqual "testInterferenceEdges - Fibonnaci"
                (sort $ t2Res)
                (sort $ getInterferenceEdges t2aasmList t2alocsList)

g2Output :: Graph ALoc
g2Output = buildInterferenceGraph t2aasmList t2alocsList

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

g2newGraph@(Graph g2Map) = newGraph
newMap = (Map.insert (ATemp 1) v1' (Map.insert (ATemp 2) v2' (Map.insert (ATemp 3) v3'
         (Map.insert (ATemp 4) v4' (Map.insert (ATemp 5) v5'
         (Map.insert (ATemp 6) v6' g2Map))))))
g2Correct = Graph newMap

test3 :: Assertion
test3 = assertEqual "testGraphConstruction - Fibonnaci"
                     g2Correct
                     g2Output

tests = [testCase "getInterferenceEdges" test1,
         testCase "getInterferenceEdges - Fibonnaci" test2,
         testCase "buildInterferenceGraph - Fibonnaci" test3
        ]



interferenceTest = tests
