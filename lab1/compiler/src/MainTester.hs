-- Tests for the c0c compiler. 

module Main where 

import Tests.Compile.Backend.LivePredicatesTest (livePredicates)
import Tests.Compile.Backend.LivenessTest (livenessTest)

import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [
         testGroup "livePredicates" livePredicates,
         testGroup "liveness" livenessTest
       ]
