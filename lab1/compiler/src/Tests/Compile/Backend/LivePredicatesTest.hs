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

test5 = assertEqual "runPredicate"
        [ATemp 1, ATemp 2]
        (runPredicate (AAsm {aAssign=[ATemp 0],
                             aOp=Mul,
                             aArgs=[ALoc $ ATemp 1, ALoc $ ATemp 2]}) 
                      [ATemp 0])

test6 = assertEqual "runPredicate"
        [ATemp 1, ATemp 5, ATemp 3]
        (runPredicate (AAsm {aAssign=[ATemp 0],
                             aOp=Nop,
                             aArgs=[ALoc $ ATemp 1]}) 
                      [ATemp 5, ATemp 3])

test7 = assertEqual "runPredicate"
        [ATemp 1, ATemp 5, ATemp 3]
        (runPredicate (AAsm {aAssign=[ATemp 0],
                             aOp=Add,
                             aArgs=[ALoc $ ATemp 1, AImm 5]}) 
                      [ATemp 5, ATemp 3])

test8 = assertEqual "runPredicate"
        [ATemp 1]
        (runPredicate (AAsm {aAssign=[ATemp 0],
                             aOp=Add,
                             aArgs=[ALoc $ ATemp 1, ALoc $ ATemp 1]}) 
                      [ATemp 1])


tests = [testCase "test1" test1
        ,testCase "test2" test2
        ,testCase "test3" test3
        ,testCase "test4" test4
        ,testCase "test5" test5
        ,testCase "test6" test6
        ,testCase "test7" test7
        ,testCase "test8" test8
        ]

livePredicates = tests
