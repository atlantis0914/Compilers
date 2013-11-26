module Tests.Compile.Backend.SpillTest (spillTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.Spill

t1Input = [AAsm {aAssign = [AReg 20], aOp = Nop, aArgs = [AImm 1]},
           AAsm {aAssign = [AReg 21], aOp = Nop, aArgs = [AImm 2]},
           AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [ALoc $ AReg 20]},
           AAsm {aAssign = [AReg 0], aOp = Add, aArgs = [ALoc $ AReg 21]}]

t1Expected = []

test1 :: Assertion
test1 = assertEqual "spill Basic test 1"
                    t1Expected
                    (spill t1Input)

spillTest = [testCase "test spillTest - Basic" test1]
