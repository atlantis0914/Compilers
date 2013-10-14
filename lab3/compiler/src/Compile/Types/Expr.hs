module Compile.Types.Expr where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType

data Base = Dec
          | Hex

data Expr = ExpInt !Integer SourcePos !Base
          | ExpBool !Bool SourcePos
          | Ident !String SourcePos
          | ExpBinOp !Op !Expr !Expr SourcePos
          | ExpRelOp !Op !Expr !Expr SourcePos
          | ExpLogOp !Op !Expr !Expr SourcePos
          | ExpPolyEq !Op !Expr !Expr SourcePos
          | ExpUnOp !Op !Expr SourcePos
          | ExpTernary !Expr !Expr !Expr SourcePos
          | ExpFnCall !String ![Expr] SourcePos

instance Show Expr where
  show (ExpInt n _ _) = show n
  show (ExpBool b _) = show b
  show (ExpBinOp op e1 e2 _) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (ExpRelOp op e1 e2 _) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (ExpPolyEq op e1 e2 _) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (ExpLogOp op e1 e2 _) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (Ident i _) = i
  show (ExpUnOp op e _) = show op ++ "(" ++ show e ++ ")"
  show (ExpTernary e1 e2 e3 _) = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
  show (ExpFnCall n _ _) = "call " ++ n
