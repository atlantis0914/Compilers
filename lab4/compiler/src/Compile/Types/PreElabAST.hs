module Compile.Types.PreElabAST where 

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.Expr
import Compile.Types.Ctrl

type ParseCtrl = PolyCtrl ParseStmt

data ParseFnList = ParseFnList ![PGDecl] SourcePos deriving Show

data PGDecl = PFDecl !ParseFDecl SourcePos
            | PFDefn !ParseFDefn SourcePos 
            | PSDecl !ParseSDecl SourcePos
            | PSDefn !ParseSDefn SourcePos
            | PTypeDef !IdentType !IdentType !SourcePos deriving Show

data ParseFDefn = ParseFDefn {pfnName :: !String,
                              pfnArgs :: ![String],
                              pfnArgTypes :: ![IdentType],
                              pfnReturnType :: !IdentType,
                              pfnBody :: !ParseAST,
                              pfnPos :: SourcePos} deriving Show

-- fnName, fnArgs, fnArgTypes, fnReturnType
data ParseFDecl = ParseFDecl {pdeclName :: !String,
                              pdeclArgs :: ![String],
                              pdeclArgTypes :: ![IdentType],
                              pdeclReturnType :: !IdentType,
                              pdeclIsLibrary :: !Bool,
                              pdeclPos :: SourcePos} deriving Show

data ParseSDecl = ParseSDecl String SourcePos deriving Show

data ParseSDefn = ParseSDefn {pdefnName :: !String,
                              pdefnFields :: [(IdentType, String)]} deriving Show
    

data ParseAST = ParseAST !ParseStmt SourcePos 

-- A simple wrapper around memory and idents. We enforce that 
-- the memory locations are in fact l-values and not general exprs
-- inside of the type-checker. 
data PLValue = PLId !String
             | PLMem !Mem

data ParseStmt = PAsgn {pasgnName :: !PLValue,
                        pasgnOp :: !AsgnOp,
                        pasgnExpr :: !Expr,
                        pasgnShadow :: !Bool,
                        pasgnPos :: SourcePos}
               | PDecl !String !IdentType SourcePos !(Maybe ParseStmt)
               | PCtrl !ParseCtrl
               | PBlock ![ParseStmt]
               | PExpr !Expr

instance Show ParseAST where
  show (ParseAST stmt _) =
    "int main () {\n" ++ show stmt ++ "}\n"

instance Show ParseStmt where
  show (PAsgn s o e b _) = "\t" ++ s ++ " " ++ (show o) ++ "=" ++ " " ++ show e ++ ";"
  show (PDecl i t _ Nothing) = "\t" ++ (show t) ++ " " ++ i ++ ";"
  show (PDecl i t _ (Just st')) = "\t" ++ "decl " ++ (show t) ++ " " ++  i ++ " as " ++ show st'
  show (PCtrl c) = show c
  show (PBlock stmts) = "{\n" ++ (unlines (map show stmts)) ++ "\n" ++ "};" ++ "\n"
  show (PExpr expr) = show expr ++ "\n"

instance Show ParseCtrl where 
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return e1 _) = "return " ++ show e1 ++ ";"
