module Compile.Types.PostElabAST where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Control.DeepSeq

import Compile.Types.Ops
import Compile.Types.IdentType
import Compile.Types.Expr
import Compile.Types.Ctrl

import qualified Data.Map as Map

type Ctrl = PolyCtrl Stmt Expr

data FnList = FnList ![GDecl] SourcePos

data GDecl = GFDecl !FDecl SourcePos
           | GFDefn !FDefn SourcePos
           | GSDecl !SDecl SourcePos
           | GSDefn !SDefn SourcePos
           | GTypeDef !IdentType !IdentType SourcePos

data FDefn = FDefn {fnName :: !String,
                    fnArgs :: ![String],
                    fnArgTypes :: ![IdentType],
                    fnReturnType :: !IdentType,
                    fnBody :: !AST,
                    fnPos :: SourcePos}

-- fnName, fnArgs, fnArgTypes, fnReturnType
data FDecl = FDecl {gdeclName :: !String,
                    gdeclArgs :: ![String],
                    gdeclArgTypes :: ![IdentType],
                    gdeclReturnType :: !IdentType,
                    gdeclIsLibrary :: !Bool,
                    gdeclPos :: SourcePos} deriving Show

data SDecl = SDecl !String SourcePos

-- A map from the fieldName -> (fieldSize, fieldOffset)
type StructOffsets = Map.Map String (Int, Int)

data SDefn = SDefn {structName :: !String,
                    structFields :: ![(IdentType, String)],
                    -- A map from the fieldName -> (fieldSize, fieldOffset)
                    structTypes :: !(Map.Map String IdentType),
                    structOffsets :: !StructOffsets,
                    structAlignment :: !Int, -- 0 mod 4 or 0 mod 8
                    structSize :: !Int,
                    structPos :: SourcePos}

data AST = AST !Stmt SourcePos

data LValue = LExpr !Expr SourcePos

data Stmt = Asgn !LValue !AsgnOp !Expr !Bool SourcePos
          | Decl {declName :: !String,
                  declTyp :: !IdentType,
                  declPos :: SourcePos,
                  declScope :: !Stmt}
          | Ctrl !Ctrl
          | Block ![Stmt]
          | Expr !Expr
          | SNop

instance Show FnList where
  show (FnList gdecls _) = concatMap (\s -> show s ++ "\n") gdecls

instance Show GDecl where
  show (GFDecl fdecl _) = show fdecl
  show (GFDefn fdefn _) = show fdefn
  show (GSDecl sdecl _) = show sdecl
  show (GSDefn sdefn _) = show sdefn
  show (GTypeDef t1 t2 _) = "typedef " ++ show t1 ++ " as " ++ show t2

instance Show FDefn where
  show (FDefn name args argTypes retType body _) =
    (show retType) ++ " " ++ name ++ "("
    ++ (concatMap (\(typ, n) -> show typ ++ " " ++ n ++ ",") (zip argTypes args))
    ++ ")" ++ show body

instance Show AST where
  show (AST stmt _) = ""
--    "{\n" ++ show stmt ++ "}\n"

maybeShow Nothing = ""
maybeShow (Just o) = show o

instance Show SDecl where
  show (SDecl s _) = "struct " ++ s ++ ";" ++ "\n"

instance Show SDefn where
  show (SDefn name fields _ offs align size _) = "struct " ++ name ++ "{\n" ++
    (concatMap (\(typ, s) -> "\t" ++ (show typ) ++ " " ++ s ++ ";\n") fields) ++ "}\n" ++
    "stuctinfo: " ++ name ++ "\n" ++
    "totalSize: " ++ (show size) ++ "\n" ++
    "largestAlign: " ++ (show align) ++ "\n" ++
    (concatMap (\(_,s) -> "\t" ++ s ++ (show (Map.lookup s offs)) ++ ";\n") fields)

-- instance Show AMem where
--   show (Dot s id _) = "(" ++ show s ++ "." ++ id ++ ")"
--   show (Arrow s id _) = "(" ++ show s ++ "->" ++ id ++ ")"
--   show (Star s _) = "(" ++ "*" ++ show s ++ ")"
--   show (ArrayRef s e _) = "(" ++ show s ++ "[" ++ show e ++ "])"

instance Show LValue where
  show (LExpr e _) = show e

instance Show Stmt where
  show (Asgn s o e b _) = "\t" ++ show s ++ " " ++ (maybeShow o) ++ "=" ++ " " ++ show e ++ ";"
  show (Decl i t _ innerS) = "\t" ++ (show t) ++ " " ++ i ++ ";" ++ show innerS
  show (Ctrl c) = "\t" ++ show c
  show (Block stmts) = "{\n" ++ (unlines (map show stmts)) ++ "};" ++ "\n"
  show (Expr expr) = show expr ++ "\n"
  show (SNop) = "N-n-n-nope\n"

instance Show Ctrl where
  show (If e1 s1 s2 _) = "if(" ++ show e1 ++ ") " ++ show s1 ++ "else" ++ show s2 ++ "\n"
  show (While e1 s1 _) = "while(" ++ show e1 ++ ")\n" ++ show s1
  show (Return Nothing _) = "return " ++ ";"
  show (Return (Just e1) _) = "return " ++ show e1 ++ ";"

instance NFData FnList where
  rnf (FnList decls _) = decls `deepseq` ()

instance NFData GDecl where
  rnf (GFDecl fdecl _) = fdecl `deepseq` ()
  rnf (GFDefn fdefn _) = fdefn `deepseq` ()
  rnf (GSDecl sdecl _) = sdecl `deepseq` ()
  rnf (GSDefn sdefn _) = sdefn `deepseq` ()
  rnf (GTypeDef t1 t2 _) = t1 `deepseq` t2 `deepseq` ()

instance NFData FDefn where 
  rnf (FDefn name args argTypes retType body _) = 
    name `deepseq` args `deepseq` argTypes `deepseq` retType `deepseq` body `deepseq` ()

instance NFData FDecl where 
  rnf (FDecl name args argTypes retType isLib _) = 
    name `deepseq` args `deepseq` argTypes `deepseq` retType `deepseq` isLib `deepseq` ()

instance NFData SDecl where
  rnf (SDecl n _) = n `deepseq` ()

instance NFData SDefn where 
  rnf (SDefn name fields fieldTypes offsets align size _) = 
    name `deepseq` fields `deepseq` fieldTypes `deepseq` offsets `deepseq` align `deepseq` size `deepseq` ()

instance NFData AST where 
  rnf (AST stmt _) = stmt `deepseq` ()

instance NFData Stmt where
  rnf (Asgn lv as e b _) = lv `deepseq` e `deepseq` b `deepseq` ()
  rnf (Decl name typ _ scp) = name `deepseq` typ `deepseq` scp `deepseq` ()
  rnf (Ctrl c) = c `deepseq` ()
  rnf (Block stmts) = stmts `deepseq` ()
  rnf (Expr e) = e `deepseq` ()
  rnf (SNop) = ()

instance NFData LValue where 
  rnf (LExpr e _) = e `deepseq` ()

instance NFData Ctrl where 
  rnf (If e s1 s2 _) = e `deepseq` s1 `deepseq` s2 `deepseq` ()
  rnf (While e s1 _) = e `deepseq` s1 `deepseq` ()
  rnf (Assert e _) = e `deepseq` ()
  rnf (Return me _) = me `deepseq` ()


