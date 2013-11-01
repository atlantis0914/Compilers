module Compile.Types.IRAST where

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.IRExpr
import Compile.Types.Ctrl

import qualified Data.Map as Map

data IRFnList = IRFnList [IRDecl]

data IRDecl = IRFDefn IRFuncDef
            | IRSDefn IRStructDef

data IRFuncDef = IRFuncDef {funcName :: !String,
                            funcArgs :: ![String],
                            funcArgTypes :: ![IdentType],
                            funcRetType :: !IdentType,
                            funcBody :: !IRAST,
                            funcArgSizes :: ![Int]}

data IRStructDef = IRStructDef {strctName :: !String,
                                strctFields :: ![(IdentType, String)],
                                strctTypes :: Map.Map String IdentType,
                                strctOffsets :: Map.Map String (Int, Int),
                                strctAlign :: Int, -- 0 mod 4 or 0 mod 8
                                strctSize :: Int}

data IRAST = IRAST IRStmt

type IRCtrl = PolyCtrl IRStmt IRExpr

data IRStmt = IRAsgn IRExpr AsgnOp IRExpr
            | IRDecl {ideclName :: String,
                      ideclTyp :: IdentType,
                      ideclScope :: IRStmt}
            | IRCtrl IRCtrl
            | IRBlock [IRStmt]
            | IRExpr IRExpr

instance Show IRFnList where
  show (IRFnList gdecls) = concatMap (\s -> show s ++ "\n") gdecls

instance Show IRDecl where
  show (IRFDefn fdefn) = show fdefn
  show (IRSDefn sdefn) = show sdefn

instance Show IRFuncDef where
  show (IRFuncDef name args argTypes retType body) =
    (show retType) ++ " " ++ name ++ "("
    ++ (concatMap (\(typ, n) -> show typ ++ " " ++ n ++ ",") (zip argTypes args))
    ++ ")" ++ show body

instance Show IRStructDef where
  show (IRStructDef name fields _ offs align size) = "struct " ++ name ++ "{\n" ++
    (concatMap (\(typ, s) -> "\t" ++ (show typ) ++ " " ++ s ++ ";\n") fields) ++ "}\n" ++
    "stuctinfo: " ++ name ++ "\n" ++
    "totalSize: " ++ (show size) ++ "\n" ++
    "largestAlign: " ++ (show align) ++ "\n" ++
    (concatMap (\(_,s) -> "\t" ++ s ++ (show (Map.lookup s offs)) ++ ";\n") fields)

instance Show IRAST where
  show (IRAST stmt) =
    "{\n" ++ show stmt ++ "}\n"

mShow Nothing = ""
mShow (Just o) = show o

instance Show IRStmt where
  show (IRAsgn e1 o e2) = "\t" ++ show e1 ++ " " ++ (mShow o) ++ "=" ++ " " ++ show e2 ++ ";"
  show (IRDecl i t innerS) = "\t" ++ (show t) ++ " " ++ i ++ ";" ++ show innerS
  show (IRCtrl c) = "\t" ++ show c
  show (IRBlock stmts) = "{\n" ++ (unlines (map show stmts)) ++ "};" ++ "\n"
  show (IRExpr expr) = "\t" ++ show expr ++ "\n"

instance Show IRCtrl where
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return Nothing _) = "return " ++ ";"
  show (Return (Just e1) _) = "return " ++ show e1 ++ ";"

