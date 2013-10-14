module Compile.Types.Ctrl where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType

import Compile.Types.Expr

data PolyCtrl s = If !Expr !s !s SourcePos
                | While !Expr !s SourcePos
                | Assert !Expr SourcePos
                | Return !(Maybe Expr) SourcePos
