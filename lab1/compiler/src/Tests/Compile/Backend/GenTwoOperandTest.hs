module Tests.Compile.Backend.GenTwoOperandTest (genTwoOperandTest) where 

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.GenTwoOperand

t1List = [AAsm {aAssign = [ATemp 3], aOp = Add, aArgs = [ALoc(ATemp 2), ALoc(ATemp 1)]}
         ]

t1Expected = [AAsm {aAssign = [ATemp 3], aOp = Nop, aArgs = [ALoc(ATemp 2)]},
              AAsm {aAssign = [ATemp 3], aOp = Add, aArgs = [ALoc(ATemp 3), ALoc(ATemp 1)]}
             ]

test1 :: Assertion
test1 = assertEqual "test threeToTwo - Two Temps"
                    t1Expected
                    (genTwoOperand t1List)

t2List = [AAsm {aAssign = [ATemp 1], aOp = Nop, aArgs = [AImm 1]}]

t2Expected = [AAsm {aAssign = [ATemp 1], aOp = Nop, aArgs = [AImm 1]}]

test2 :: Assertion
test2 = assertEqual "threeToTwo - One Immediate Move"
                     t2Expected
                     (genTwoOperand t2List)

t3List = [AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [AImm 1]}]
t3Expected = [AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [AImm 1]}]

test3 :: Assertion
test3 = assertEqual "threeToTwo - One Immediate Move to Register"
                     t3Expected
                     (genTwoOperand t3List)

t4List = [AAsm {aAssign = [ATemp 5], aOp = Add, aArgs = [ALoc(ATemp 4), ALoc(ATemp 3)]},
          AAsm {aAssign = [ATemp 6], aOp = Nop, aArgs = [ALoc(ATemp 5)]}]
t4Expected = [AAsm {aAssign = [ATemp 5], aOp = Nop, aArgs = [ALoc(ATemp 4)]},
              AAsm {aAssign = [ATemp 5], aOp = Add, aArgs = [ALoc(ATemp 5), ALoc(ATemp 3)]},
              AAsm {aAssign = [ATemp 6], aOp = Nop, aArgs = [ALoc(ATemp 5)]}]

test4 :: Assertion
test4 = assertEqual "threeToTwo - Larger Test"  
                     t4Expected 
                     (genTwoOperand t4List)

t5List = [AAsm {aAssign = [ATemp 3], aOp = Add, aArgs = [ALoc(ATemp 4), ALoc(ATemp 3)]}]
t5Expected = [AAsm {aAssign = [ATemp 3], aOp = Add, aArgs = [ALoc(ATemp 4), ALoc(ATemp 3)]}]

test5 :: Assertion
test5 = assertEqual "threeToTwo - Test d <- a + d"
                     t5Expected 
                     (genTwoOperand t5List)

genTwoOperandTest = [testCase "test threeToTwo - Two Temps" test1,
                     testCase "threeToTwo - One Immediate Move" test2,
                     testCase "test ThreeToTwo - One Immediate Move to Register" test3,
                     testCase "test ThreeToTwo - Larger Test" test4,
                     testCase "test ThreeToTwo - Test d <- a + d" test5]
