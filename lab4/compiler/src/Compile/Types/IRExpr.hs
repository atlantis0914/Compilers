module Compile.Types.IRExpr where

import Compile.Types.Ops
import Compile.Types.IdentType

data Base = Dec
          | Hex

data IROffset = IROffset Int

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
            | IRExpAlloc IdentType IROffset
            | IRExpAllocArray IdentType IRExpr IROffset
-- We add back type information for [], . , and *. 
            | IRExpArraySubscript IRExpr IRExpr IdentType IROffset
            | IRExpFieldSelect IRExpr String IdentType IROffset
            | IRExpDereference IRExpr IdentType
