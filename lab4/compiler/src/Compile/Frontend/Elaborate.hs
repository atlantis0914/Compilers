module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Util.IdentTypeUtil

import Compile.Frontend.Expand
import Compile.Frontend.ElaborateType
import Compile.Frontend.ElaborateStruct

-- Takes a parse function list and elaborates it into a post-elab 
-- function list. 
elaborate :: ParseFnList -> Either String FnList
elaborate (ParseFnList decls pos) = 
  let
    (elab, tMap, sMap) = foldl elaboratePGDecls ([], baseIdentTypeMap, Map.empty) decls
  in
    elab `seq` tMap `seq` sMap `seq` Right $ FnList elab pos


elaboratePGDecls :: ([GDecl], TypeDefs, StructDefs) -> PGDecl -> ([GDecl], TypeDefs, StructDefs)
elaboratePGDecls (convDecls, typeMap, sMap) pgdecl@(PTypeDef _ _ _) =
  let
    typeMap' = checkTypeDef pgdecl typeMap
  in
    (convDecls ++ [elaboratePGDecl pgdecl typeMap sMap], typeMap', sMap)

elaboratePGDecls (convDecls, typeMap, sMap) pgdecl@(PSDecl _ _) = 
  let
    sMap' = checkStructDecl pgdecl sMap
    typeMap' = addToTypeSpace pgdecl typeMap
  in
    (convDecls ++ [elaboratePGDecl pgdecl typeMap sMap], typeMap', sMap')

elaboratePGDecls (convDecls, typeMap, sMap) pgdecl@(PSDefn _ _) = 
  let
    sMap' = checkStructDefn pgdecl sMap
    typeMap' = addToTypeSpace pgdecl typeMap
  in
    (convDecls ++ [elaboratePGDecl pgdecl typeMap sMap'], typeMap', sMap')

elaboratePGDecls (convDecls, typeMap, sMap) pgdecl = 
  (convDecls ++ [elaboratePGDecl pgdecl typeMap sMap], typeMap, sMap)

-- Either errors if we have multiple typedefs of s2, or inserts 
-- s2 into the typeMap using s1 as 
checkTypeDef :: PGDecl -> TypeDefs -> TypeDefs
checkTypeDef (PTypeDef IVoid s2 pos) _ = 
  error ("Cannot alias IVoid - can only be used as return val")
checkTypeDef (PTypeDef s1 s2 pos) typeMap = 
  typeMap `seq` case (Map.lookup s2 typeMap) of
    Just _ -> error ("Multiple typedef of " ++ (show s1) ++ " at " ++ (show pos))
    Nothing -> (Map.insert s2 (typeMap Map.! s1) typeMap)

addToTypeSpace :: PGDecl -> TypeDefs -> TypeDefs 
addToTypeSpace (PSDecl (ParseSDecl name _) _) typeMap = 
  Map.insert typeName typeName typeMap
  where
    typeName = IStruct (ITypeDef name)

addToTypeSpace (PSDefn (ParseSDefn name _ _) _) typeMap = 
  if ((Map.lookup typeName typeMap) == Nothing)
    then Map.insert typeName typeName typeMap
    else error ("Multiplie definition of struct : " ++ name) 
  where 
    typeName = IStruct (ITypeDef name)

maybeAddReturn :: IdentType -> AST -> AST
maybeAddReturn IVoid (AST (Block stmts) pos) = 
  (AST (Block (stmts ++ [Ctrl (Return Nothing pos)])) pos)
maybeAddReturn _ ast = ast
 
-- Global Decl. 
elaboratePGDecl :: PGDecl -> TypeDefs -> StructDefs ->  GDecl
elaboratePGDecl (PFDefn (ParseFDefn {pfnName = name,
                                     pfnArgs = args,
                                     pfnArgTypes = argTypes,
                                     pfnReturnType = return,
                                     pfnBody = body,
                                     pfnPos = pos}) defnPos) typeDefs _ = 
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

elaboratePGDecl (PFDecl (ParseFDecl name args argTypes return isL pos) dPos) typeDefs _ = 
  let
    nName = checkTDIdent typeDefs name
    nArgs = checkTDIdentList typeDefs args
    nArgTypes = elaborateTDIdentTypes typeDefs argTypes 
    nReturn = elaborateTDIdentType typeDefs return
  in
    GFDecl (FDecl nName nArgs nArgTypes nReturn isL pos) dPos

-- Can add struct field-namespace checking here. 
elaboratePGDecl (PSDecl (ParseSDecl s p1) p2) typeDefs _ = (GSDecl (SDecl s p2) p1)
elaboratePGDecl (PSDefn sdefn@(ParseSDefn s fields p1) p2) typeDefs sDefs = 
  let
    init = generateStructDefn typeDefs sDefs sdefn
  in
    GSDefn (checkStructFields init) p2

elaboratePGDecl (PTypeDef s1 s2 pos) typeDefs _ = GTypeDef s1 s2 pos

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
elabParseStmt td (PAsgn s a e b p) = Asgn (plValToLVal s) a e b p
elabParseStmt td (PDecl s t p Nothing) = 
  Decl {declName = checkTDIdent td s, 
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

plValToLVal :: PLValue -> LValue 
plValToLVal (PLId s p) = (LId s p)
plValToLVal (PLMem mem p) = (LMem (pMemToAMem mem) p )

pMemToAMem :: ParseMem -> AMem
pMemToAMem (Dot s id p) = Dot (plValToLVal s) id p
pMemToAMem (Arrow s id p) = Arrow (plValToLVal s) id p
pMemToAMem (Star s p) = Star (plValToLVal s) p
pMemToAMem (ArrayRef s e p) = ArrayRef (plValToLVal s) e p
