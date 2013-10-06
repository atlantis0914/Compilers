{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines the AST we parse to
-}
module Compile.Types.AST where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType

data ParseFnList = ParseFnList [GDecl]

data GDecl = FDecl ParseFDecl
           | FDefn ParseFDefn
           | TypeDef String String deriving Show

data ParseFDefn = ParseFDefn {fnName :: String,
                              fnArgs :: [String],
                              fnArgTypes :: [String],
                              fnReturnType :: String,
                              fnBody :: ParseAST} deriving Show

-- fnName, fnArgs, fnArgTypes, fnReturnType
data ParseFDecl = ParseFDecl String [String] [String] String deriving Show

data ParseAST = ParseAST ParseStmt SourcePos 

data ParseStmt = PAsgn String AsgnOp Expr SourcePos
               | PDecl String IdentType SourcePos (Maybe ParseStmt)
               | PCtrl ParseCtrl
               | PBlock [ParseStmt]
               | PExpr Expr

data AST = AST Stmt SourcePos

data Stmt = Asgn String AsgnOp Expr SourcePos
          | Decl {declName :: String,
                  declTyp :: IdentType,
                  declPos :: SourcePos,
                  declScope :: Stmt}
          | Ctrl Ctrl
          | Block [Stmt]
          | Expr Expr
          | SNop

data PolyCtrl s = If Expr s s SourcePos
                | While Expr s SourcePos
                | Return Expr SourcePos

type Ctrl = PolyCtrl Stmt
type ParseCtrl = PolyCtrl ParseStmt

isDecl :: Stmt -> Bool
isDecl (Decl {}) = True
isDecl _ = False

data Expr = ExpInt Integer SourcePos Base
          | ExpBool Bool SourcePos
          | Ident String SourcePos
          | ExpBinOp Op Expr Expr SourcePos
          | ExpRelOp Op Expr Expr SourcePos
          | ExpLogOp Op Expr Expr SourcePos
          | ExpPolyEq Op Expr Expr SourcePos
          | ExpUnOp Op Expr SourcePos
          | ExpTernary Expr Expr Expr SourcePos

type AsgnOp = Maybe Op

data Base = Dec
          | Hex

instance Show ParseAST where
  show (ParseAST stmt _) =
    "int main () {\n" ++ show stmt ++ "}\n"

instance Show ParseStmt where
  show (PAsgn s o e _) = "\t" ++ s ++ " " ++ mShow o ++ "=" ++ " " ++ show e ++ ";"
  show (PDecl i t _ Nothing) = "\t" ++ (show t) ++ " " ++ i ++ ";"
  show (PDecl i t _ (Just st')) = "\t" ++ "decl " ++ (show t) ++ " " ++  i ++ " as " ++ show st'
  show (PCtrl c) = show c
  show (PBlock stmts) = "{\n" ++ (unlines (map show stmts)) ++ "\n" ++ "};" ++ "\n"
  show (PExpr expr) = show expr ++ "\n"

instance Show AST where
  show (AST stmt _) =
    "int main () {\n" ++ show stmt ++ "}\n"

instance Show Stmt where
  show (Asgn s o e _) = "\t" ++ s ++ " " ++ mShow o ++ "=" ++ " " ++ show e ++ ";"
  show (Decl i t _ innerS) = "\t" ++ (show t) ++ " " ++ i ++ ";" ++ show innerS
  show (Ctrl c) = show c
  show (Block stmts) = "{\n" ++ (unlines (map show stmts)) ++ "\n" ++ "};" ++ "\n"
  show (Expr expr) = show expr ++ "\n"

instance Show Ctrl where 
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return e1 _) = "return " ++ show e1 ++ ";"

instance Show ParseCtrl where 
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return e1 _) = "return " ++ show e1 ++ ";"

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

mShow Nothing = ""
mShow (Just x) = show x
