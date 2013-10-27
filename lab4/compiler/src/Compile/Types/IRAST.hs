module Compile.Types.PreIRAST where 

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.IRExpr
import Compile.Types.Ctrl

import qualified Data.Map as Map


data IRFnList = IRFnList [IRDecl] 

data IRDecl = IRFDefn FuncDef
            | IRSDefn StructDef

data IRFuncDef = IRFuncDef {funcName :: !String,
                            funcArgs :: ![String],
                            funcArgTypes :: ![IdentType],
                            funcRetType :: !IdentType,
                            funcBody :: !IAST}

data IRStructDef = IRStructDef {strctName :: !String,
                                strctFields :: ![(IdentType, String)],
                                strctTypes :: Map.Map String IdentType,
                                strctOffsets :: Map.Map String (Int, Int),
                                strctAlign :: Int, -- 0 mod 4 or 0 mod 8
                                strctSize :: Int}

data IRAST = IRAST IRStmt 

type IRCtrl = PolyCtrl IStmt

data IRStmt = IRAsgn IRExpr AsgnOp IRExpr Bool 
            | IRDecl {ideclName :: String,
                      ideclTyp :: IdentType,
                      ideclScope :: IStmt}
            | IRCtrl ICtrl
            | IRBlock [IStmt]
            | IRExpr IRExpr
