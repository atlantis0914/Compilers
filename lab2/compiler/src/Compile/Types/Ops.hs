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
        | Decr
        | Incr
        | LogicalNot
        | BitwiseNot
        | Lt
        | Lte
        | Gt
        | Gte
        | Equ
        | Neq
        | And
        | Or
        | BitwiseAnd
        | BitwiseXOr
        | BitwiseOr
        | RShift
        | LShift
        | Nop deriving Eq

instance Show Op where
  show Mul = "*"
  show Add = "+"
  show Sub = "Sub"
  show Div = "/"
  show Neg = "Neg"
  show Mod = "%"
  show Nop = "[nop]"
  show Decr = "Decr"
  show Incr = "Incr"
  show LogicalNot = "!"
  show BitwiseNot = "~"
  show Lt = "<"
  show Lte = "<="
  show Gt = ">"
  show Gte = ">="
  show Equ = "=="
  show Neq = "!="
  show And = "&&"
  show Or = "||"
  show BitwiseAnd = "&"
  show BitwiseOr = "|"
  show RShift = ">>"
  show LShift = "<<"

data COp = Ret deriving (Show, Eq)
