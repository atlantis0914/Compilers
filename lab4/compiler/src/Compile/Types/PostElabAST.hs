module Compile.Types.PostElabAST where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.Expr
import Compile.Types.Ctrl

import qualified Data.Map as Map

type Ctrl = PolyCtrl Stmt

data FnList = FnList ![GDecl] SourcePos deriving Show

data GDecl = GFDecl !FDecl SourcePos
           | GFDefn !FDefn SourcePos
           | GSDecl !SDecl SourcePos
           | GSDefn !SDefn SourcePos
           | GTypeDef !IdentType !IdentType SourcePos deriving Show

data FDefn = FDefn {fnName :: !String,
                    fnArgs :: ![String],
                    fnArgTypes :: ![IdentType],
                    fnReturnType :: !IdentType,
                    fnBody :: !AST,
                    fnPos :: SourcePos} deriving Show

-- fnName, fnArgs, fnArgTypes, fnReturnType
data FDecl = FDecl {gdeclName :: !String,
                    gdeclArgs :: ![String],
                    gdeclArgTypes :: ![IdentType],
                    gdeclReturnType :: !IdentType,
                    gdeclIsLibrary :: !Bool,
                    gdeclPos :: SourcePos} deriving Show

data SDecl = SDecl !String SourcePos deriving Show

-- A map from the fieldName -> (fieldSize, fieldOffset)
type StructOffsets = Map.Map String (Int, Int)

data SDefn = SDefn {structName :: !String,
                    structFields :: ![(IdentType, String)],
                    -- A map from the fieldName -> (fieldSize, fieldOffset)
                    structOffsets :: StructOffsets,
                    structAlignment :: Int, -- 0 mod 4 or 0 mod 8
                    structSize :: Int, 
                    structPos :: SourcePos} deriving Show  

data AST = AST !Stmt SourcePos

type AMem = Mem LValue

data LValue = LId !String SourcePos
            | LMem !AMem SourcePos

data Stmt = Asgn !LValue !AsgnOp !Expr !Bool SourcePos
          | Decl {declName :: !String,
                  declTyp :: !IdentType,
                  declPos :: SourcePos,
                  declScope :: !Stmt}
          | Ctrl !Ctrl
          | Block ![Stmt]
          | Expr !Expr
          | SNop

instance Show AST where
  show (AST stmt _) =
    "int main () {\n" ++ show stmt ++ "}\n"

maybeShow Nothing = ""
maybeShow (Just o) = show o

instance Show Stmt where
  show (Asgn s o e b _) = "\t" ++ " " ++ (maybeShow o) ++ "=" ++ " " ++ show e ++ ";"
  show (Decl i t _ innerS) = "\t" ++ (show t) ++ " " ++ i ++ ";" ++ show innerS
  show (Ctrl c) = show c
  show (Block stmts) = "{\n" ++ (unlines (map show stmts)) ++ "\n" ++ "};" ++ "\n"
  show (Expr expr) = show expr ++ "\n"

instance Show Ctrl where
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return e1 _) = "return " ++ show e1 ++ ";"

