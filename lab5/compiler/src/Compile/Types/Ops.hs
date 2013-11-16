{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Abstract Assembly operations
-}
module Compile.Types.Ops where

type AsgnOp = Maybe Op

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
        | Ae
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
        | Select -- .
        | FDereference -- ->
        | PDereference -- *
        | PArrayRef -- [expr]
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
  show BitwiseXOr = "^"
  show RShift = ">>"
  show LShift = "<<"
  show Select = "."
  show FDereference = "->"
  show PDereference = "*"
  show PArrayRef = "[]"
