module Compile.Types.IRAST where 

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.IRExpr
import Compile.Types.Ctrl

import qualified Data.Map as Map

data IRFnList = IRFnList [IRDecl] deriving Show

data IRDecl = IRFDefn IRFuncDef
            | IRSDefn IRStructDef deriving Show

data IRFuncDef = IRFuncDef {funcName :: !String,
                            funcArgs :: ![String],
                            funcArgTypes :: ![IdentType],
                            funcRetType :: !IdentType,
                            funcBody :: !IRAST} deriving Show

data IRStructDef = IRStructDef {strctName :: !String,
                                strctFields :: ![(IdentType, String)],
                                strctTypes :: Map.Map String IdentType,
                                strctOffsets :: Map.Map String (Int, Int),
                                strctAlign :: Int, -- 0 mod 4 or 0 mod 8
                                strctSize :: Int} deriving Show

data IRAST = IRAST IRStmt deriving Show

type IRCtrl = PolyCtrl IRStmt IRExpr 

data IRStmt = IRAsgn IRExpr AsgnOp IRExpr
            | IRDecl {ideclName :: String,
                      ideclTyp :: IdentType,
                      ideclScope :: IRStmt}
            | IRCtrl IRCtrl
            | IRBlock [IRStmt]
            | IRExpr IRExpr deriving Show

instance Show IRCtrl where
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return Nothing _) = "return " ++ ";"
  show (Return (Just e1) _) = "return " ++ show e1 ++ ";"

