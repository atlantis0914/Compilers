module Compile.Types.Ctrl where

import Text.ParserCombinators.Parsec.Pos (SourcePos, newPos)

import Compile.Types.Ops
import Compile.Types.IdentType

import Compile.Types.Expr

dummyPos = newPos "dummy" 0 0

data PolyCtrl s e = If !e !s !s SourcePos
                  | While !e !s SourcePos
                  | Assert !e SourcePos
                  | Return !(Maybe e) SourcePos


