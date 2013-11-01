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
          | AMem Int Bool deriving (Show, Eq, Ord)

data ACtrl = ARet AVal
           | ALabel Int
           | AIf AVal Int (Maybe String)
           | AGoto Int deriving (Show, Eq)
