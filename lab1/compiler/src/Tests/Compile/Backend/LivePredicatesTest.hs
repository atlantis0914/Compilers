module Tests.Compile.Backend.LivePredicatesTest (livePredicates) where 

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.LivePredicates

test1 :: Assertion
test1 = assertEqual "extractLocs" 
                    [ATemp 1] 
                    (extractLocs [AImm 1, ALoc $ ATemp 1])

test2 = assertEqual "extractLocs" 
                    [] 
                    (extractLocs [AImm 1, AImm 2])
 
test3 = assertEqual "extractLocs" 
                    [ATemp 2] 
                    (extractLocs [ALoc $ ATemp 2])

test4 = assertEqual "runPredicate"
                    [ATemp 0, ATemp 1]
                    (runPredicate (AAsm {aAssign=[AReg 0],
                                         aOp=Mul,
                                         aArgs=[ALoc $ ATemp 0, ALoc $ ATemp 1]})
                                   [ATemp 3])
tests = [testCase "test1" test1
        ,testCase "test2" test2
        ,testCase "test3" test3
        ,testCase "test4" test4
        ]
-- TestLabel "test2" test2,
--                  TestLabel "test3" test3, TestLabel "test4" test4]

livePredicates = tests
