module Tests.Compile.Backend.LivenessTest (livenessTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.Liveness

return_test = assertEqual "liveness on a single return"
              [[ATemp 0]]
              (liveness [AAsm {aAssign=[AReg 0],
                               aOp=Nop,
                               aArgs=[ALoc $ ATemp 0]}])

live_test1 = assertEqual "liveness on a single return"
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
                              aArgs=[ALoc $ ATemp 2]}])

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
  ,[ATemp 4, ATemp 3]
  ,[ATemp 5]
  ]

large_test :: Assertion
large_test = assertEqual "test large"
        t2alocsList
        (liveness t2aasmList)

tests = [testCase "return_test" return_test
        ,testCase "live_test1" live_test1
        ,testCase "large test" large_test
        ]

livenessTest = tests
