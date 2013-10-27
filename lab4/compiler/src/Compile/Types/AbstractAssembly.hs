{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines a flat abstract assembly.
-}
module Compile.Types.AbstractAssembly where

import Compile.Types.Ops


data FnAAsm = AAFDefn [AAsm] String Int
            | AAFDecl String deriving Show

data AAsm = AAsm {aAssign :: [ALoc]
                 ,aOp     :: Op
                 ,aArgs   :: [AVal]
                 }
          | ACtrl ACtrl
          | AFnCall String ALoc [ALoc] [ALoc]
          | AComment String deriving (Show, Eq)

data AVal = ALoc ALoc
          | ABool Bool
          | AImm Int deriving (Show, Eq)

data ALoc = AReg Int
          | ATemp Int
          | ASpill
          | AIndex
          | AArg Int
          | APtr ALoc (Maybe ALoc) Int
          | AMem Int deriving (Show, Eq)

data ACtrl = ARet AVal
           | ALabel Int
           | AIf AVal Int
           | AGoto Int deriving (Show, Eq)

instance Ord ALoc where
  (AReg _) `compare` (ATemp _) = GT
  (ATemp _) `compare` (AReg _) = LT
  (AReg i) `compare` (AReg i') = i `compare` i'
  (ATemp i) `compare` (ATemp i') = i `compare` i'
  (APtr _ _) `compare` (AReg _) = GT
  (APtr _ _) `compare` (ATemp _) = GT
  (AReg _) `compare` (APtr _ _) = LT
  (ATemp _) `compare` (APtr _ _) = LT
  (APtr loc _) `compare` (APtr loc' _) = loc `compare` loc'

