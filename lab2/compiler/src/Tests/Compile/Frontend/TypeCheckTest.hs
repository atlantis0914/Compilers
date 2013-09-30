module Tests.Compile.Frontend.TypeCheckTest (typeCheckTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import Compile.Types.AST
import Compile.Frontend.TypeCheck
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Text.Parsec.Pos as P

identMap = Map.fromList [("a", IBool), ("b", IInt), ("c", IBool), ("d", IInt)]
context = (identMap, True)
context2 = (Map.empty, True)

sourcePos = P.initialPos "a"
expInt = ExpInt 4 sourcePos Dec
expBool = ExpBool True sourcePos
identA = Ident "a" sourcePos
identB = Ident "b" sourcePos
identC = Ident "c" sourcePos
identD = Ident "d" sourcePos
expBinOp1 = ExpBinOp Mul identB expInt sourcePos
expBinOp2 = ExpBinOp Mul expInt expInt sourcePos
expBinOp3 = ExpBinOp Mul identA expInt sourcePos
expBinOp4 = ExpBinOp Mul expBool expInt sourcePos
expBinOp5 = ExpBinOp Mul identB identD sourcePos
expRelOp1 = ExpRelOp Lt identB expInt sourcePos
expRelOp2 = ExpRelOp Lt expInt expInt sourcePos
expRelOp3 = ExpRelOp Lt identA expInt sourcePos
expRelOp4 = ExpRelOp Lt expBool expInt sourcePos
expRelOp5 = ExpRelOp Lt identB identD sourcePos
expRelOp6 = ExpRelOp Lt identB expBinOp1 sourcePos
expLogOp1 = ExpLogOp And identA expBool sourcePos
expLogOp2 = ExpLogOp And expBool expBool sourcePos
expLogOp3 = ExpLogOp And identA expInt sourcePos
expLogOp4 = ExpLogOp And expBool expInt sourcePos
expLogOp5 = ExpLogOp And identA expRelOp1 sourcePos
expUnOp1 = ExpUnOp Neg identA sourcePos
expUnOp2 = ExpUnOp Neg identB sourcePos
expTernary1 = ExpTernary identA identB identB sourcePos
expTernary2 = ExpTernary identB identB identB sourcePos
expTernary3 = ExpTernary identA identA identB sourcePos
expTernary4 = ExpTernary identA identB identA sourcePos

decl1 = Decl "b" IInt sourcePos SNop
decl2 = Decl "d" IInt sourcePos SNop
decl3 = Decl "a" IBool sourcePos SNop
if1 = Ctrl $ If identA block2 block3 sourcePos
if2 = Ctrl $ If identA block2 block4 sourcePos
if3 = Ctrl $ If identA block7 block8 sourcePos
ret1 = Ctrl $ Return identB sourcePos
block1 = Block [decl3, decl1, decl2, if1, ret1]
asgn1 = Asgn "b" Nothing expInt sourcePos
asgn2 = Asgn "d" Nothing expInt sourcePos
asgn3 = Asgn "a" Nothing expInt sourcePos
block2 = Block [asgn1]
block3 = Block [asgn2]
block4 = Block [asgn3]
block5 = Block [decl3, decl1, if2, ret1]
block6 = Block [decl3, if3, decl1, ret1]
block7 = Block [decl1]
block8 = Block [decl2]

test1 :: Assertion
test1 = assertEqual "test1"
                    (Just IInt)
                    (checkExprType context expInt)

test2 :: Assertion
test2 = assertEqual "test2"
                    (Just IBool)
                    (checkExprType context expBool)

test3 :: Assertion
test3 = assertEqual "test3"
                    (Just IBool)
                    (checkExprType context identA)

test4 :: Assertion
test4 = assertEqual "test4"
                    (Just IInt)
                    (checkExprType context identB)

test5 :: Assertion
test5 = assertEqual "test5"
                    (Just IInt)
                    (checkExprType context expBinOp1)

test6 :: Assertion
test6 = assertEqual "test6"
                    (Just IInt)
                    (checkExprType context expBinOp2)

test7 :: Assertion
test7 = assertEqual "test7"
                    (Nothing)
                    (checkExprType context expBinOp3)

test8 :: Assertion
test8 = assertEqual "test8"
                    (Nothing)
                    (checkExprType context expBinOp4)

test9 :: Assertion
test9 = assertEqual "test9"
                    (Just IInt)
                    (checkExprType context expBinOp5)

test10 :: Assertion
test10 = assertEqual "test10"
                     (Just IBool)
                     (checkExprType context expRelOp1)

test11 :: Assertion
test11 = assertEqual "test11"
                     (Just IBool)
                     (checkExprType context expRelOp2)

test12 :: Assertion
test12 = assertEqual "test12"
                     (Nothing)
                     (checkExprType context expRelOp3)

test13 :: Assertion
test13 = assertEqual "test13"
                     (Nothing)
                     (checkExprType context expRelOp4)

test14 :: Assertion
test14 = assertEqual "test14"
                     (Just IBool)
                     (checkExprType context expRelOp5)

test15 :: Assertion
test15 = assertEqual "test15"
                     True
                     (snd $ checkStmtValid context2 block1)

test16 :: Assertion
test16 = assertEqual "test16"
                     False
                     (snd $ checkStmtValid context2 block4)

test17 :: Assertion
test17 = assertEqual "test17"
                     True
                     (snd $ checkStmtValid context2 block6)

typeCheckTest = [testCase "1" test1,
                 testCase "2" test2,
                 testCase "3" test3,
                 testCase "4" test4,
                 testCase "5" test5,
                 testCase "6" test6,
                 testCase "7" test7,
                 testCase "8" test8,
                 testCase "9" test9,
                 testCase "10" test10,
                 testCase "11" test11,
                 testCase "12" test12,
                 testCase "13" test13,
                 testCase "14" test14,
                 testCase "15" test15,
                 testCase "16" test16,
                 testCase "17" test17]
