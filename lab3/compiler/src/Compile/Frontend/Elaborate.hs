module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

import Compile.Frontend.Expand

type TypeDefs = Map.Map String IdentType

-- Takes a parse function list and elaborates it into a post-elab 
-- function list. 
elaborate :: ParseFnList -> Either String FnList
-- elaborate (ParseFnList decls pos) = Right $ FnList (map elaboratePGDecl decls) pos
elaborate (ParseFnList decls pos) = 
  let
    (elab, map) = foldl elaboratePGDecls ([], Map.empty) decls
  in
    Right $ FnList elab pos

-- We currently do not do anything with the additional typedef information
-- we pass around. Currently, the additional parameter is passed only to ensure
-- no namespace conflicts within typedefs. 
elaboratePGDecls :: ([GDecl], Map.Map String IdentType) -> PGDecl -> 
                        ([GDecl], Map.Map String IdentType)
elaboratePGDecls (convDecls, typeMap) pgdecl@(PFDefn _ _) = 
  (convDecls ++ [elaboratePGDecl pgdecl typeMap], typeMap)

elaboratePGDecls (convDecls, typeMap) pgdecl@(PFDecl _ _) = 
  (convDecls ++ [elaboratePGDecl pgdecl typeMap], typeMap)

elaboratePGDecls (convDecls, typeMap) pgdecl@(PTypeDef _ _ _) =
  let
    typeMap' = checkTypeDef pgdecl typeMap
  in
    (convDecls ++ [elaboratePGDecl pgdecl typeMap], typeMap')

-- Either errors if we have multiple typedefs of s2, or inserts 
-- s2 into the typeMap using s1 as 
checkTypeDef :: PGDecl -> TypeDefs -> TypeDefs
checkTypeDef (PTypeDef s1 s2 pos) typeMap = 
  case (Map.lookup s2 typeMap) of
    Just _ -> error ("Multiple typedef of " ++ s1 ++ " at " ++ (show pos))
    Nothing -> (Map.insert s2 (typeMap Map.! s1) typeMap)
 
-- Global Decl. 
elaboratePGDecl :: PGDecl -> Map.Map String IdentType -> GDecl
elaboratePGDecl (PFDefn (ParseFDefn {pfnName = name,
                                     pfnArgs = args,
                                     pfnArgTypes = argTypes,
                                     pfnReturnType = return,
                                     pfnBody = body,
                                     pfnPos = pos}) defnPos) typeDefs = 
  GFDefn (FDefn {fnName = name,
                 fnArgs = args,
                 fnArgTypes = argTypes,
                 fnReturnType = return,
                 fnBody = elaborateParseAST body,
                 fnPos = pos}) defnPos

elaboratePGDecl (PFDecl (ParseFDecl name args argTypes return isL pos) dPos) typeDefs = 
  GFDecl (FDecl name args argTypes return isL pos) dPos

elaboratePGDecl (PTypeDef s1 s2 pos) typeDefs = GTypeDef s1 s2 pos

-- Converts a parse level AST into a post-elaboration AST. Expects the encapsulated
-- ParseStatement to be a PBlock. 
elaborateParseAST (ParseAST (PBlock stmts) p) = 
  let
    expanded = expandPStatements stmts
    elaborated = elabParseBlock expanded
  in
    AST elaborated p

-- Converts a parse-block into a single post-elaboration statement. 
elabParseBlock :: [ParseStmt] -> Stmt
elabParseBlock stmts = elabParseBlock' (Block []) stmts

-- Converts a single parseStmt into a stmt, mutually calling elabParseBlock'
-- if we observe a nested block, and elabParseCtrl if we observe a nested ctrl
elabParseStmt :: ParseStmt -> Stmt
elabParseStmt (PAsgn s a e p) = Asgn s a e p
elabParseStmt (PDecl s t p Nothing) = Decl {declName = s, 
                                            declTyp = t, 
                                            declPos = p, 
                                            declScope = SNop}
elabParseStmt (PDecl s t p (Just _)) = error "shouldnt get here just"
elabParseStmt (PCtrl c) = Ctrl (elabParseCtrl c)
elabParseStmt (PExpr e) = Expr e
elabParseStmt (PBlock stmts) = elabParseBlock' (Block []) stmts

-- Converts a Parse level control flow statement into a post-elab
-- control flow statement, elaborating inner statements using elabParseStmt. 
elabParseCtrl :: ParseCtrl -> Ctrl
elabParseCtrl (If e ps1 ps2 pos) = If e (elabParseStmt ps1) (elabParseStmt ps2) pos
elabParseCtrl (While e ps1 pos) = While e (elabParseStmt ps1) pos
elabParseCtrl (Assert e pos) = Assert e pos
elabParseCtrl (Return e pos) = Return e pos

-- Converts a list of parse level statements into a single statement. We
-- recursively convert statements and append them to the post-elab block's 
-- inner list. 
elabParseBlock' :: Stmt -> [ParseStmt] -> Stmt 
elabParseBlock' curblock [] = curblock
elabParseBlock' (Block curStmts) ((PDecl s t pos Nothing):xs) = 
  Block $ curStmts ++ [Decl {declName = s,
                             declTyp = t,
                             declPos = pos,
                             declScope = elabParseBlock' (Block []) xs}
                      ]
elabParseBlock' (Block curStmts) (x:xs) = elabParseBlock' (Block (curStmts ++ [elabParseStmt x])) xs
