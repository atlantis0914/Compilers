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

data ALoc = AReg Int Bool
          | ATemp Int Bool
          | ASpill Bool
          | AUtil Bool
          | AIndex Bool
          | AArg Int Bool
          | APtr ALoc (Maybe ALoc) Int Int Bool
          | AMem Int Bool deriving (Show, Eq)

data ACtrl = ARet AVal
           | ALabel Int
           | AIf AVal Int (Maybe String)
           | AGoto Int deriving (Show, Eq)

instance Ord ALoc where
  (AReg _ _) `compare` (ATemp _ _) = GT
  (ATemp _ _) `compare` (AReg _ _) = LT
  (AReg i _) `compare` (AReg i' _) = i `compare` i'
  (ATemp i _) `compare` (ATemp i' _) = i `compare` i'
  (APtr _ _ _ _ _) `compare` (AReg _ _) = GT
  (APtr _ _ _ _ _) `compare` (ATemp _ _) = GT
  (AReg _ _) `compare` (APtr _ _ _ _ _) = LT
  (ATemp _ _) `compare` (APtr _ _ _ _ _) = LT
  (APtr loc _ _ _ _) `compare` (APtr loc' _ _ _ _) = loc `compare` loc'
  (ASpill _) `compare` (ASpill _) = EQ
  (ASpill _) `compare` (AReg _ _) = GT
  (ASpill _) `compare` (ATemp _ _) = GT
  (ASpill _) `compare` (APtr _ _ _ _ _) = GT
  (AReg _ _) `compare` (ASpill _) = LT
  (ATemp _ _) `compare` (ASpill _) = LT
  (APtr _ _ _ _ _) `compare` (ASpill _) = LT
  (AIndex _) `compare` (AIndex _) = EQ
  (AIndex _) `compare` (ASpill _) = GT
  (AIndex _) `compare` (AReg _ _) = GT
  (AIndex _) `compare` (ATemp _ _) = GT
  (AIndex _) `compare` (APtr _ _ _ _ _) = GT
  (AReg _ _) `compare` (AIndex _) = LT
  (ATemp _ _) `compare` (AIndex _) = LT
  (APtr _ _ _ _ _) `compare` (AIndex _) = LT
  (ASpill _) `compare` (AIndex _) = LT
  (AUtil _) `compare` (AUtil _) = EQ
  (AUtil _) `compare` (AIndex _) = GT
  (AUtil _) `compare` (ASpill _) = GT
  (AUtil _) `compare` (AReg _ _) = GT
  (AUtil _) `compare` (ATemp _ _) = GT
  (AUtil _) `compare` (APtr _ _ _ _ _) = GT
  (AReg _ _) `compare` (AUtil _) = LT
  (ATemp _ _) `compare` (AUtil _) = LT
  (APtr _ _ _ _ _) `compare` (AUtil _) = LT
  (ASpill _) `compare` (AUtil _) = LT
  (AIndex _) `compare` (AUtil _) = LT

