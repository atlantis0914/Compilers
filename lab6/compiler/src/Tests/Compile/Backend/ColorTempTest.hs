module Tests.Compile.Backend.ColorTempTest (colorTempTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.ColorTemp
import qualified Data.Map as Map

coloring = Map.fromList [(ATemp 0, Color 2), (ATemp 1, Color 0), (ATemp 2, Color 1)]

aasms1 = [
  AAsm {aAssign = [ATemp 0], aOp = Nop, aArgs = [AImm 1]},
  AAsm {aAssign = [ATemp 1], aOp = Nop, aArgs = [AImm 1]},
  AAsm {aAssign = [ATemp 2], aOp = Add, aArgs = [ALoc $ ATemp 2, ALoc $ ATemp 1]}]

expected1 = [
  AAsm {aAssign = [AReg 2], aOp = Nop, aArgs = [AImm 1]},
  AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [AImm 1]},
  AAsm {aAssign = [AReg 1], aOp = Add, aArgs = [ALoc $ AReg 1, ALoc $ AReg 0]}]

test1 :: Assertion
test1 = assertEqual "test simple"
                    expected1
                    (colorTemps aasms1 coloring)

tests = [testCase "simple_test" test1]
colorTempTest = tests
