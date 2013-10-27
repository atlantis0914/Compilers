module Compile.Types.IRExpr where

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.Expr

data IRExpr = IRExpInt Integer Base
            | IRExpBool Bool
            | IRIdent String
            | IRExpBinOp Op IRExpr IRExpr
            | IRExpRelOp Op IRExpr IRExpr
            | IRExpLogOp Op IRExpr IRExpr
            | IRExpPolyEq Op IRExpr IRExpr
            | IRExpUnOp Op IRExpr
            | IRExpTernary IRExpr IRExpr IRExpr
            | IRExpFnCall String [IRExpr]
            | IRExpNull
            | IRExpAlloc IdentType Int
            | IRExpAllocArray IdentType IRExpr Int
-- We add back type information for [], . , and *.
            | IRExpArraySubscript IRExpr IRExpr IdentType Int
            | IRExpFieldSelect IRExpr String IdentType Int
            | IRExpDereference IRExpr IdentType deriving Show 
