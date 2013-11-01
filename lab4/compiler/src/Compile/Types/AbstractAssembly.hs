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
          | AUtil
          | AIndex
          | AArg Int
          | APtr ALoc (Maybe ALoc) Int Int
          | AMem Int deriving (Show, Eq, Ord)

data ACtrl = ARet AVal
           | ALabel Int
           | AIf AVal Int (Maybe String)
           | AGoto Int deriving (Show, Eq)
