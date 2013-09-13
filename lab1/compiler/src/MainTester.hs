-- Tests for the c0c compiler. 

module Main where 

import Tests.Compile.Backend.LivePredicatesTest (livePredicates)

import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [
         testGroup "livePredicates" livePredicates
       ]
