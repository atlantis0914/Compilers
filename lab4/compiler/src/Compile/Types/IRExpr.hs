module Compile.Types.IRExpr where

import Compile.Types.Ops
import Compile.Types.IdentType

data Base = Dec
          | Hex

data IROffset = IROffset Int

data IRExpr = ExpInt Integer Base
            | ExpBool Bool 
            | Ident String 
            | ExpBinOp Op IRExpr IRExpr
            | ExpRelOp Op IRExpr IRExpr
            | ExpLogOp Op IRExpr IRExpr
            | ExpPolyEq Op IRExpr IRExpr
            | ExpUnOp Op IRExpr
            | ExpTernary IRExpr IRExpr IRExpr
            | ExpFnCall String [IRExpr]
            | ExpNull
            | ExpAlloc IdentType
            | ExpAllocArray IdentType IRExpr
-- We add back type information for [], . , and *. 
            | ExpArraySubscript IRExpr IRExpr IdentType IROffset
            | ExpFieldSelect IRExpr String IdentType IROffset 
            | ExpDereference IRExpr IdentType
