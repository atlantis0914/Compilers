{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Abstract Assembly operations
-}
module Compile.Types.Ops where

data Op = Mul
        | Add
        | Sub
        | Div
        | Neg
        | Mod
        | Nop

instance Show Op where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Neg = "-"
  show Mod = "%"
  show Nop = "[nop]"

data COp = Ret deriving Show
