module Tests.Compile.Backend.GenAsmTest(genAsmTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Backend.GenAsm

prelude = [".globl _main\n", "_main:\n"]
epilogue = ["ret\n"]

t1Input = [AAsm {aAssign = [AReg 1], aOp = Nop, aArgs = [AImm 1]},
           AAsm {aAssign = [AReg 2], aOp = Nop, aArgs = [AImm 2]},
           AAsm {aAssign = [AReg 0], aOp = Nop, aArgs = [ALoc $ AReg 1]},
           AAsm {aAssign = [AReg 0], aOp = Add, aArgs = [ALoc $ AReg 2]}]

t1Expected = prelude ++ ["movl $1, %rbx\n",
                         "movl $2, %rcx\n",
                         "movl %rbx, %rax\n",
                         "addl %rcx, %rax\n"] ++ epilogue

test1 :: Assertion
test1 = assertEqual "genAsm Basic test 1"
                    t1Expected
                    (genAsm t1Input)

genAsmTest = [testCase "test genAsm - Basic" test1]

