module Compile.Types.IRExpr where

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.Expr

data IRExpr = IRExpInt Integer Base
            | IRExpBool Bool
            | IRIdent String Int
            | IRExpBinOp Op IRExpr IRExpr
            | IRExpRelOp Op IRExpr IRExpr
            | IRExpLogOp Op IRExpr IRExpr
            | IRExpPolyEq Op IRExpr IRExpr
            | IRExpUnOp Op IRExpr
            | IRExpTernary IRExpr IRExpr IRExpr
            | IRExpFnCall String [IRExpr] Int
            | IRExpNull
            | IRExpAlloc IdentType Int
            | IRExpAllocArray IdentType IRExpr Int
-- We add back type information for [], . , and *.
            | IRExpArraySubscript IRExpr IRExpr IdentType Int
            | IRExpFieldSelect IRExpr String IdentType Int
            | IRExpDereference IRExpr IdentType

instance Show IRExpr where
  show (IRExpInt n _) = show n
  show (IRExpBool b) = show b
  show (IRIdent i) = i
  show (IRExpNull) = "NULL"
  show (IRExpBinOp op e1 e2) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (IRExpRelOp op e1 e2) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (IRExpPolyEq op e1 e2) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (IRExpLogOp op e1 e2) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (IRExpUnOp op e) = show op ++ "(" ++ show e ++ ")"
  show (IRExpTernary e1 e2 e3) = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
  show (IRExpFnCall n elist) = "call " ++ n ++ "(" ++ (concatMap (\e -> show e ++ ",") elist) ++ ")"
  show (IRExpAlloc typ off) = "alloc(" ++ show typ ++ "," ++ show off ++ ")"
  show (IRExpAllocArray typ e off) = "alloc_array(" ++ show typ ++ "," ++ show e ++ "," ++ show off ++ ")"
  show (IRExpArraySubscript e1 e2 typ off) = "(" ++ show e1 ++ "[" ++ show e2 ++ "]" ++ "," ++ show typ ++ "," ++ show off ++ ")"
  show (IRExpFieldSelect e1 field typ off) = "(" ++ show e1 ++ "." ++ show field ++ "," ++ show typ ++ "," ++ show off ++ ")"
  show (IRExpDereference e1 typ) = "(" ++ "*" ++ show e1 ++ "," ++ show typ ++ ")"
