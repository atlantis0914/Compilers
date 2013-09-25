{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines the AST we parse to
-}
module Compile.Types.AST where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType

data AST = Block [Stmt] SourcePos

data Stmt = Asgn String AsgnOp Expr SourcePos
          | Decl {declName :: String, 
                  declType :: IdentType, 
                  declPos :: SourcePos, 
                  extraAsgn :: Maybe Stmt}
          | Return Expr SourcePos

isDecl :: Stmt -> Bool 
isDecl (Decl {}) = True
isDecl _ = False 

data Expr = ExpInt Integer SourcePos Base
          | Ident String SourcePos
          | ExpBinOp Op Expr Expr SourcePos
          | ExpUnOp Op Expr SourcePos

type AsgnOp = Maybe Op

data Base = Dec
          | Hex

-- Note to the student: You will probably want to write a new pretty printer
-- using the module Text.PrettyPrint.HughesPJ from the pretty package
-- This is a quick and dirty pretty printer.
-- Once that is written, you may find it helpful for debugging to switch
-- back to the deriving Show instances.

instance Show AST where
  show (Block stmts _) =
    "int main () {\n" ++ unlines (map show stmts) ++ "}\n"

instance Show Stmt where
  show (Return e _) = "\treturn " ++ show e ++ ";"
  show (Decl i t _ Nothing) = "\t" ++ (show t) ++  i ++ ";"
  show (Decl i t _ (Just st')) = "\t" ++ "decl " ++ (show t) ++ i ++ " as " ++ show st'
  show (Asgn i op e _) = "\t" ++ i ++ " " ++ mShow op ++ "=" ++ " " ++ show e ++ ";"

instance Show Expr where
  show (ExpInt n _ _) = show n
  show (ExpBinOp op e1 e2 _) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (Ident i _) = i
  show (ExpUnOp op e _) = show op ++ "(" ++ show e ++ ")"

mShow Nothing = ""
mShow (Just x) = show x
