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

t3aasmList =
  [AAsm {aAssign = [ATemp 0], aOp = Nop, aArgs = [AImm 7]}
  ,AAsm {aAssign = [ATemp 1], aOp = Nop, aArgs = [AImm 6]}
  ,AAsm {aAssign = [ATemp 2], aOp = Mul, aArgs = [ALoc(ATemp 0), ALoc(ATemp 1)]}
  ,AAsm {aAssign = [ATemp 3], aOp = Nop, aArgs = [AImm 2]}
  ,AAsm {aAssign = [ATemp 4], aOp = Sub, aArgs = [ALoc(ATemp 2), ALoc(ATemp 3)]}
  ,AAsm {aAssign = [ATemp 5], aOp = Nop, aArgs = [AImm 1]}
  ,AAsm {aAssign = [ATemp 6], aOp = Nop, aArgs = [AImm 0]}
  ,AAsm {aAssign = [ATemp 7], aOp = Nop, aArgs = [AImm 1]}
  ,ACtrl $ ALabel 0
  ,AAsm {aAssign = [ATemp 4], aOp = Add, aArgs = [ALoc(ATemp 4), ALoc(ATemp 5)]}
  ,AAsm {aAssign = [ATemp 6], aOp = Add, aArgs = [ALoc(ATemp 6), ALoc(ATemp 7)]}
  ,ACtrl $ AIf (ALoc $ ATemp 4) 0
  ,AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [ALoc(ATemp 6)]}
  ]

t3alocsList =
  [[]
  ,[ATemp 0]
  ,[ATemp 0, ATemp 1]
  ,[ATemp 2]
  ,[ATemp 2, ATemp 3]
  ,[ATemp 4]
  ,[ATemp 4, ATemp 5]
  ,[ATemp 4, ATemp 5, ATemp 6]
  ,[ATemp 4, ATemp 5, ATemp 6, ATemp 7]
  ,[ATemp 4, ATemp 5, ATemp 6, ATemp 7]
  ,[ATemp 6, ATemp 7, ATemp 4, ATemp 5]
  ,[ATemp 6, ATemp 4, ATemp 5, ATemp 7]
  ,[ATemp 6]
  ]

t4aasmList =
  [AAsm {aAssign = [ATemp 0], aOp = Nop, aArgs = [AImm 7]}
  ,ACtrl $ AGoto 0
  ,AAsm {aAssign = [ATemp 0], aOp = Nop, aArgs = [AImm 7]}
  ,ACtrl $ ALabel 0
  ,AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [ALoc $ ATemp 0]}
  ]

t4alocsList =
  [[]
  ,[ATemp 0]
  ,[]
  ,[ATemp 0]
  ,[ATemp 0]
  ]

large_test :: Assertion
large_test = assertEqual "test large"
        t2alocsList
        (liveness t2aasmList)

loop_test :: Assertion
loop_test = assertEqual "test loop"
        t3alocsList
        (liveness t3aasmList)

goto_test :: Assertion
goto_test = assertEqual "test goto"
        t4alocsList
        (liveness t4aasmList)


tests = [testCase "return_test" return_test
        ,testCase "live_test1" live_test1
        ,testCase "large test" large_test
        ,testCase "loop test" loop_test
        ,testCase "goto test" goto_test
        ]

livenessTest = tests
