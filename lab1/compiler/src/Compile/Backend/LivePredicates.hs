module Compile.Backend.LivePredicates where

import Compile.Types
import Data.List

extractLocs :: [AVal] -> [ALoc]
extractLocs [] = []
extractLocs (val:vals) =
  case val of ALoc loc -> loc : extractLocs vals
              _ -> extractLocs vals

runPredicate ::  AAsm -> [ALoc] -> [ALoc]
runPredicate (AAsm {aAssign = [AReg 0], aArgs = args}) _ = 
  nub $ extractLocs args
runPredicate (AAsm {aAssign = assigns, aArgs = args}) locs =
  (nub $ extractLocs args) `union` (locs \\ assigns)
