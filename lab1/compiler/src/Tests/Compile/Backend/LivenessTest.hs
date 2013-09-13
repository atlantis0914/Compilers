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

tests = [testCase "return_test" return_test
        ,testCase "live_test1" live_test1
        ]

livenessTest = tests
