module Tests.Compile.Frontend.CheckReturnTest (checkReturnTest) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Compile.Types
import qualified Text.Parsec.Pos as P
import Compile.Frontend.CheckReturn

sourcePos = P.initialPos "a"
identA = Ident "a" sourcePos
ret1 = Ctrl $ Return identA sourcePos
decl1 = Decl "b" IInt sourcePos Nothing
decl2 = Decl "d" IInt sourcePos Nothing
decl3 = Decl "a" IBool sourcePos Nothing
if1 = Ctrl $ If identA block2 block3 sourcePos
if2 = Ctrl $ If identA block2 block4 sourcePos
block1 = Block [decl3, decl1, decl2, if1]
block2 = Block [ret1]
block3 = Block [ret1]
block4 = Block [decl1]
block5 = Block [decl3, decl1, decl2, if2]

test1 :: Assertion
test1 = assertEqual "test1"
                    True
                    (checkReturnStmt block1)

test2 :: Assertion
test2 = assertEqual "test2"
                    False
                    (checkReturnStmt block5)

checkReturnTest = [testCase "1" test1,
                   testCase "2" test2]
