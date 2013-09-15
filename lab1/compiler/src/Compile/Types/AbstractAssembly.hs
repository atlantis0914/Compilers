{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines a flat abstract assembly.
-}
module Compile.Types.AbstractAssembly where

import Compile.Types.Ops

data AAsm = AAsm {aAssign :: [ALoc]
                 ,aOp     :: Op
                 ,aArgs   :: [AVal]
                 }
          | ACtrl COp AVal
          | AComment String deriving (Show, Eq)

data AVal = ALoc ALoc
          | AImm Int deriving (Show, Eq)

data ALoc = AReg Int
          | ATemp Int deriving (Show, Eq)

instance Ord ALoc where 
  (AReg _) `compare` (ATemp _) = GT
  (ATemp _) `compare` (AReg _) = LT
  (AReg i) `compare` (AReg i') = i `compare` i'
  (ATemp i) `compare` (ATemp i') = i `compare` i'
