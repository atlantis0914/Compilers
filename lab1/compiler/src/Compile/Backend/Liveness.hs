
module Compile.Backend.Liveness where

import Compile.Types
import Compile.Backend.LivePredicates
import Data.List
import Test.HUnit

liveness :: [AAsm] -> [[ALoc]]
liveness aasms = take (length aasms) (scanr runPredicate [] aasms) 

-- Unit Tests

return_test = TestCase (assertEqual "liveness on a single return"
                       [[ATemp 0]]
                       (liveness [AAsm {aAssign=[AReg 0], 
                                        aOp=Nop, 
                                        aArgs=[ALoc $ ATemp 0]}]))

live_test1 = TestCase (assertEqual "liveness on a single return"
                      [[], [ATemp 0], [ATemp 0, ATemp 1], [ATemp 2]]
                      (liveness [AAsm {aAssign=[ATemp 0],
                                       aOp=Nop,
                                       aArgs=[AImm 5]},
                                 AAsm {aAssign=[ATemp 1],
                                       aOp=Nop,
                                       aArgs=[AImm 2]},
                                 AAsm {aAssign=[ATemp 2],
                                       aOp=Add,
                                       aArgs=[ALoc $ ATemp 0, ALoc $ ATemp 1]},
                                 AAsm {aAssign=[AReg 0], 
                                       aOp=Nop, 
                                       aArgs=[ALoc $ ATemp 2]}]))

live_tests = TestList [TestLabel "return test" return_test,
                       TestLabel "simple prog test" live_test1]
