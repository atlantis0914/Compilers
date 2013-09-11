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
  extractLocs args
runPredicate (AAsm {aAssign = assigns, aArgs = args}) locs =
  extractLocs args ++ (locs \\ assigns)

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

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2,
                  TestLabel "test3" test3, TestLabel "test4" test4]
