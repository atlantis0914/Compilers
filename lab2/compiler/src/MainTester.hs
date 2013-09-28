-- Tests for the c0c compiler.

module Main where

import Tests.Compile.Backend.SpillTest(spillTest)
import Tests.Compile.Frontend.TypeCheckTest(typeCheckTest)
import Tests.Compile.Frontend.CheckReturnTest(checkReturnTest)

import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [
         testGroup "spillTest" spillTest,
         testGroup "typeCheck" typeCheckTest,
         testGroup "checkReturn" checkReturnTest
       ]
