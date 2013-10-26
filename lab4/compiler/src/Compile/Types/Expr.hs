module Compile.Types.Expr where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType

data Base = Dec
          | Hex

data Mem s = Dot !s !String SourcePos
           | Arrow !s !String SourcePos
           | Star !s SourcePos
           | ArrayRef !s !Expr SourcePos 

type ExprMem = Mem Expr
 
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
          | ExpNull SourcePos
          | ExpAlloc !IdentType SourcePos
          | ExpAllocArray !IdentType !Expr SourcePos
          | ExpBinMem !Op !Expr !Expr SourcePos
          | ExpUnMem !Op !Expr SourcePos
          | ExpMem !ExprMem SourcePos

instance Show ExprMem where 
  show (Dot s id _) = "(" ++ show s ++ "." ++ id ++ ")"
  show (Arrow s id _) = "(" ++ show s ++ "->" ++ id ++ ")"
  show (Star s _) = "(" ++ "*" ++ show s ++ ")"
  show (ArrayRef s e _) = "(" ++ show s ++ "[" ++ show e ++ "])"

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
  show (ExpFnCall n elist _) = "call " ++ n ++ "(" ++ (concatMap (\e -> show e ++ ",") elist) ++ ")"
  show (ExpNull _) = "NULL"
  show (ExpAlloc i _) = "alloc(" ++ show i ++ ")"
  show (ExpAllocArray i e _) = "alloc_array(" ++ show i ++ "," ++ show e ++ ")"
  show (ExpBinMem op e1 e2 _) = "(" ++ show e1 ++ ")" ++ show op ++ "(" ++ show e2 ++ ")"
  show (ExpUnMem op e1 _) = "(" ++ show op ++ "(" ++ show e1 ++ ")" ++ ")" 
  show (ExpMem mem _) = "(" ++ show mem ++ ")"
