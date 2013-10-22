-- Tests for the c0c compiler.

module Main where

import Tests.Compile.Frontend.TypeCheckTest(typeCheckTest)
import Tests.Compile.Frontend.CheckReturnTest(checkReturnTest)
import Tests.Compile.Backend.LivenessTest(livenessTest)

import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [
         testGroup "livenessTest" livenessTest,
         testGroup "typeCheck" typeCheckTest,
         testGroup "checkReturn" checkReturnTest
       ]
