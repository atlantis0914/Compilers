module Compile.Backend.LivePredicates where

import Compile.Types
import Data.List
import Test.HUnit

extractLocs :: [AVal] -> [ALoc]
extractLocs [] = []
extractLocs (val:vals) =
  case val of ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

runPredicate ::  AAsm -> [ALoc] -> [ALoc]
runPredicate (AAsm {aAssign = [AReg 0], aArgs = args}) _ = 
  nub $ extractLocs args
runPredicate (AAsm {aAssign = assigns, aArgs = args}) locs =
  (nub $ extractLocs args) `union` (locs \\ assigns)

-- Unit Tests
test1 = TestCase (assertEqual "extractLocs" 
                 [ATemp 1] 
                 (extractLocs [AImm 1, ALoc $ ATemp 1]))

test2 = TestCase (assertEqual "extractLocs" 
                 [] 
                 (extractLocs [AImm 1, AImm 2]))

test3 = TestCase (assertEqual "extractLocs" 
                 [ATemp 2] 
                 (extractLocs [ALoc $ ATemp 2]))

test4 = TestCase (assertEqual "runPredicate"
                  [ATemp 0, ATemp 1]
                  (runPredicate (AAsm {aAssign=[AReg 0],
                                       aOp=Mul,
                                       aArgs=[ALoc $ ATemp 0, ALoc $ ATemp 1]}) 
                                [ATemp 3]))

test5 = TestCase (assertEqual "runPredicate"
                  [ATemp 1, ATemp 2]
                  (runPredicate (AAsm {aAssign=[ATemp 0],
                                       aOp=Mul,
                                       aArgs=[ALoc $ ATemp 1, ALoc $ ATemp 2]}) 
                                [ATemp 0]))

test6 = TestCase (assertEqual "runPredicate"
                  [ATemp 1, ATemp 5, ATemp 3]
                  (runPredicate (AAsm {aAssign=[ATemp 0],
                                       aOp=Nop,
                                       aArgs=[ALoc $ ATemp 1]}) 
                                [ATemp 5, ATemp 3]))

test7 = TestCase (assertEqual "runPredicate"
                  [ATemp 1, ATemp 5, ATemp 3]
                  (runPredicate (AAsm {aAssign=[ATemp 0],
                                       aOp=Add,
                                       aArgs=[ALoc $ ATemp 1, AImm 5]}) 
                                [ATemp 5, ATemp 3]))

test8 = TestCase (assertEqual "runPredicate"
                  [ATemp 1]
                  (runPredicate (AAsm {aAssign=[ATemp 0],
                                       aOp=Add,
                                       aArgs=[ALoc $ ATemp 1, ALoc $ ATemp 1]}) 
                                [ATemp 1]))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2,
                  TestLabel "test3" test3, TestLabel "test4" test4,
                  TestLabel "test5" test5, TestLabel "test6" test6,
                  TestLabel "test7" test7]
