module Tests.Compile.Backend.GenAsmTest(genAsmTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.GenAsm

prelude = [".globl __c0_main\n", "__c0_main:\n"]
epilogue = ["  ret\n"]

t1Input = [AAsm {aAssign = [AReg 1], aOp = Nop, aArgs = [AImm 1]},
           AAsm {aAssign = [AReg 2], aOp = Nop, aArgs = [AImm 2]},
           AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [ALoc $ AReg 1]},
           AAsm {aAssign = [AReg 0], aOp = Add, aArgs = [ALoc $ AReg 2]}]

t1Expected = prelude ++ ["  movl $1, %ebx\n",
                         "  movl $2, %ecx\n",
                         "  movl %ebx, %eax\n",
                         "  addl %ecx, %eax\n"] ++ epilogue

test1 :: Assertion
test1 = assertEqual "genAsm Basic test 1"
                    t1Expected
                    (genAsm t1Input)

t2Input = [AAsm {aAssign = [AReg 20], aOp = Nop, aArgs = [AImm 1]},
           AAsm {aAssign = [AReg 21], aOp = Nop, aArgs = [AImm 2]},
           AAsm {aAssign = [AReg 23], aOp = Nop, aArgs = [ALoc $ AReg 20]},
           AAsm {aAssign = [AReg 23], aOp = Add, aArgs = [ALoc $ AReg 21]}]

t2Expected = prelude ++ ["  movl $1, %ebx\n",
                         "  movl $2, %ecx\n",
                         "  movl %ebx, %eax\n",
                         "  addl %ecx, %eax\n"] ++ epilogue

test2 :: Assertion
test2 = assertEqual "genAsm Spill test 1"
                    t2Expected
                    (genAsm t2Input)
genAsmTest = [testCase "test genAsm - Basic" test1,
              testCase "test genAsm - Spill" test2]

