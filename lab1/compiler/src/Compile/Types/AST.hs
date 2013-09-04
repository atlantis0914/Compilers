{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines the AST we parse to
-}
module Compile.Types.AST where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops

data AST = Block [Decl] [Stmt] SourcePos
data Decl = Decl {declName :: String, declPos :: SourcePos}
data Stmt = Asgn String AsgnOp Expr SourcePos 
          | Return Expr SourcePos
data Expr = ExpInt Integer SourcePos
          | Ident String SourcePos
          | ExpBinOp Op Expr Expr SourcePos
          | ExpUnOp Op Expr SourcePos
type AsgnOp = Maybe Op


-- Note to the student: You will probably want to write a new pretty printer
-- using the module Text.PrettyPrint.HughesPJ from the pretty package
-- This is a quick and dirty pretty printer.
-- Once that is written, you may find it helpful for debugging to switch
-- back to the deriving Show instances.

instance Show AST where
  show (Block decls stmts _) =
    "int main () {\n" ++ (unlines $ (map show decls)
                                    ++ [""]
                                    ++ (map show stmts)) ++ "}\n"

instance Show Decl where
  show (Decl i _) = "\tint " ++ i ++ ";"

instance Show Stmt where
  show (Return e _) = "\treturn " ++ (show e) ++ ";"
  show (Asgn i op e _) = "\t" ++ i ++ " " ++ (mShow op) ++ "=" ++ " " ++ (show e) ++ ";"

instance Show Expr where
  show (ExpInt n _) = show n
  show (ExpBinOp op e1 e2 _) = "(" ++ (show e1) ++ ") " ++ (show op) ++ " (" ++ (show e2) ++ ")"
  show (Ident i _) = i
  show (ExpUnOp op e _) = (show op) ++ "(" ++ (show e) ++ ")"

mShow Nothing = ""
mShow (Just x) = show x
