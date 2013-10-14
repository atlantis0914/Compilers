module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

import Compile.Frontend.Expand
import Compile.Frontend.ElaborateType


-- type TypeDefs = Map.Map IdentType IdentType

-- Takes a parse function list and elaborates it into a post-elab 
-- function list. 
elaborate :: ParseFnList -> Either String FnList
-- elaborate (ParseFnList decls pos) = Right $ FnList (map elaboratePGDecl decls) pos
elaborate (ParseFnList decls pos) = 
  let
    (elab, map) = foldl elaboratePGDecls ([], baseIdentTypeMap) decls
  in
    Right $ FnList elab pos

elaboratePGDecls :: ([GDecl], TypeDefs) -> PGDecl -> 
                        ([GDecl], TypeDefs)
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
    Just _ -> error ("Multiple typedef of " ++ (show s1) ++ " at " ++ (show pos))
    Nothing -> (Map.insert s2 (typeMap Map.! s1) typeMap)

maybeAddReturn :: IdentType -> AST -> AST
maybeAddReturn IVoid (AST (Block stmts) pos) = 
  (AST (Block (stmts ++ [Ctrl (Return Nothing pos)])) pos)
maybeAddReturn _ ast = ast
 
-- Global Decl. 
elaboratePGDecl :: PGDecl -> TypeDefs -> GDecl
elaboratePGDecl (PFDefn (ParseFDefn {pfnName = name,
                                     pfnArgs = args,
                                     pfnArgTypes = argTypes,
                                     pfnReturnType = return,
                                     pfnBody = body,
                                     pfnPos = pos}) defnPos) typeDefs = 
  let
    nName = checkTDIdent typeDefs name
    nArgs = checkTDIdentList typeDefs args
    nArgTypes = elaborateTDIdentTypes typeDefs argTypes 
    nReturn = elaborateTDIdentType typeDefs return
    nBody = elaborateParseAST typeDefs body
    nBody' = maybeAddReturn return nBody 
  in
    GFDefn (FDefn {fnName = nName,
                   fnArgs = nArgs,
                   fnArgTypes = nArgTypes,
                   fnReturnType = nReturn,
                   fnBody = nBody',
                   fnPos = pos}) defnPos

elaboratePGDecl (PFDecl (ParseFDecl name args argTypes return isL pos) dPos) typeDefs = 
  let
    nName = checkTDIdent typeDefs name
    nArgs = checkTDIdentList typeDefs args
    nArgTypes = elaborateTDIdentTypes typeDefs argTypes 
    nReturn = elaborateTDIdentType typeDefs return
  in
    GFDecl (FDecl nName nArgs nArgTypes nReturn isL pos) dPos

elaboratePGDecl (PTypeDef s1 s2 pos) typeDefs = GTypeDef s1 s2 pos


-- Converts a parse level AST into a post-elaboration AST. Expects the encapsulated
-- ParseStatement to be a PBlock. 
elaborateParseAST typedefs (ParseAST (PBlock stmts) p) = 
  let
    expanded = expandPStatements stmts
    elaborated = elabParseBlock typedefs expanded
  in
    AST elaborated p

-- Converts a parse-block into a single post-elaboration statement. 
elabParseBlock :: TypeDefs -> [ParseStmt] -> Stmt
elabParseBlock typedefs stmts = elabParseBlock' typedefs (Block []) stmts

-- Converts a single parseStmt into a stmt, mutually calling elabParseBlock'
-- if we observe a nested block, and elabParseCtrl if we observe a nested ctrl
elabParseStmt :: TypeDefs -> ParseStmt -> Stmt
elabParseStmt td (PAsgn s a e p) = Asgn s a e p
elabParseStmt td (PDecl s t p Nothing) = 
  Decl {declName = s, 
        declTyp = elaborateTDIdentType td t, 
        declPos = p, 
        declScope = SNop}
elabParseStmt td (PDecl s t p (Just _)) = error "shouldnt get here just"
elabParseStmt td (PCtrl c) = Ctrl (elabParseCtrl td c)
elabParseStmt td (PExpr e) = Expr e
elabParseStmt td (PBlock stmts) = elabParseBlock' td (Block []) stmts

-- Converts a Parse level control flow statement into a post-elab
-- control flow statement, elaborating inner statements using elabParseStmt. 
elabParseCtrl :: TypeDefs -> ParseCtrl -> Ctrl
elabParseCtrl td (If e ps1 ps2 pos) = If e (elabParseStmt td ps1) (elabParseStmt td ps2) pos
elabParseCtrl td (While e ps1 pos) = While e (elabParseStmt td ps1) pos
elabParseCtrl td (Assert e pos) = Assert e pos
elabParseCtrl td (Return e pos) = Return e pos

-- Converts a list of parse level statements into a single statement. We
-- recursively convert statements and append them to the post-elab block's 
-- inner list. 
elabParseBlock' :: TypeDefs -> Stmt -> [ParseStmt] -> Stmt 
elabParseBlock' _ curblock [] = curblock
elabParseBlock' td (Block curStmts) ((PDecl s t pos Nothing):xs) = 
  Block $ curStmts ++ [Decl {declName = checkTDIdent td s,
                             declTyp = elaborateTDIdentType td t,
                             declPos = pos,
                             declScope = elabParseBlock' td (Block []) xs}
                      ]
elabParseBlock' td (Block curStmts) (x:xs) = elabParseBlock' td (Block (curStmts ++ [elabParseStmt td x])) xs
