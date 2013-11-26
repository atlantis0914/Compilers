module Compile.Frontend.Elaborate where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Control.DeepSeq

import Compile.Util.IdentTypeUtil

import Compile.Frontend.Expand
import Compile.Frontend.ElaborateType
import Compile.Frontend.ElaborateExpr
import Compile.Frontend.ElaborateStruct

type IdMap = Map.Map String String 

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
    elaborated = elaboratePGDecl pgdecl typeMap sMap'
    sMap'' = addStructDefn elaborated sMap
  in
    (convDecls ++ [elaboratePGDecl pgdecl typeMap sMap'], typeMap', sMap'')

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
    Nothing -> (Map.insert s2 (simplifyTypeDefdType typeMap s1) typeMap)

addToTypeSpace :: PGDecl -> TypeDefs -> TypeDefs 
addToTypeSpace (PSDecl (ParseSDecl name _) _) typeMap = typeMap
--   Map.insert typeName typeName typeMap
--   where
--     typeName = IStruct (ITypeDef name)

addToTypeSpace (PSDefn (ParseSDefn name _ _) _) typeMap = 
  if ((Map.lookup typeName typeMap) == Nothing)
    then Map.insert typeName typeName typeMap
    else error ("Multiplie definition of struct : " ++ name ++ " had " ++ 
            show (Map.lookup typeName typeMap)) 
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
    fin = checkStructFields sDefs init
  in
    fin `seq` GSDefn fin p2

elaboratePGDecl (PTypeDef s1 s2 pos) typeDefs _ = GTypeDef s1 s2 pos

-- Converts a parse level AST into a post-elaboration AST. Expects the encapsulated
-- ParseStatement to be a PBlock. 
elaborateParseAST typedefs (ParseAST (PBlock stmts) p) = 
  let
    expanded = expandPStatements stmts
    elaborated = elabParseBlock typedefs expanded
  in
    AST elaborated p

-- L4 Note : We need to throw in an identMap here. This is because we 
-- need to handle the foo * bar case, which gets parsed as decl, type=ptr(foo), name=bar. 
-- Once we have the identMap, we simply check it to see if foo and bar are decl'd - if they are
-- we convert it into Expr(foo * bar), and un-stage the decl's scope block, placing this expr at the 
-- top of it. 

-- Converts a parse-block into a single post-elaboration statement. 
elabParseBlock :: TypeDefs -> [ParseStmt] -> Stmt
elabParseBlock typedefs stmts = elabParseBlock' (Map.empty) typedefs (Block []) stmts

-- Converts a single parseStmt into a stmt, mutually calling elabParseBlock'
-- if we observe a nested block, and elabParseCtrl if we observe a nested ctrl
elabParseStmt :: IdMap -> TypeDefs -> ParseStmt -> Stmt
elabParseStmt _ td (PAsgn s a e b p) = Asgn (LExpr (elabExprTD td eLval) p) 
                                       a (elabExprTD td e) b p
  where 
    (LExpr eLval _) = plValToLVal s
elabParseStmt _ td (PDecl s t p Nothing) =  -- TODO : Get rid of this. 
  newType `seq` Decl {declName = checkTDIdent td s, 
                      declTyp = elaborateTDIdentType td t, 
                      declPos = p, 
                      declScope = SNop}
  where
    newType = elaborateTDIdentType td t
elabParseStmt id td (PDecl s t p (Just _)) = error "shouldnt get here just"
elabParseStmt id td (PCtrl c) = Ctrl (elabParseCtrl id td c)
elabParseStmt id td (PExpr e) = Expr (elabExprTD td e)
elabParseStmt id td (PBlock stmts) = elabParseBlock' id td (Block []) stmts

-- Converts a Parse level control flow statement into a post-elab
-- control flow statement, elaborating inner statements using elabParseStmt. 
elabParseCtrl :: IdMap -> TypeDefs -> ParseCtrl -> Ctrl
elabParseCtrl id td (If e ps1 ps2 pos) = If (elabExprTD td e) (elabParseStmt id td ps1) (elabParseStmt id td ps2) pos
elabParseCtrl id td (While e ps1 pos) = While (elabExprTD td e) (elabParseStmt id td ps1) pos
elabParseCtrl id td (Assert e pos) = Assert (elabExprTD td e) pos
elabParseCtrl id td (Return Nothing pos) = Return Nothing pos
elabParseCtrl id td (Return (Just e) pos) = Return (Just $ elabExprTD td e) pos

-- Converts a list of parse level statements into a single statement. We
-- recursively convert statements and append them to the post-elab block's 
-- inner list. 
elabParseBlock' :: IdMap -> TypeDefs -> Stmt -> [ParseStmt] -> Stmt 
elabParseBlock' _ _ curblock [] = curblock
elabParseBlock' id td (Block curStmts) ((decl@(PDecl s t pos Nothing)):xs) = 
  case (isDeclMultExpr id decl 0) of 
    Nothing -> Block $ curStmts ++ [Decl {declName = checkTDIdent td s,
                                          declTyp = elaborateTDIdentType td t,
                                          declPos = pos,
                                          declScope = elabParseBlock' (Map.insert (checkTDIdent td s) "poop" id) td (Block []) xs}]
    Just e -> elabParseBlock' id td (Block (curStmts ++ [Expr e])) xs
                      
elabParseBlock' id td (Block curStmts) (x:xs) = elabParseBlock' id td (Block (curStmts ++ [elabParseStmt id td x])) xs

-- Handles the context-sensitive case of parsing foo * bar, where 
-- foo and bar might be idents in scope. Checks to see if we have a parsedecl
-- with Ptr(TypeDef name) and a declname. If both names are in scope as idents,
-- we treat this as an expr parse, and convert it out to a decl inside of 
-- elabParseBlock'
isDeclMultExpr :: IdMap -> ParseStmt -> Int -> Maybe Expr
isDeclMultExpr td (PDecl declName (IPtr (ITypeDef leftIdent)) p _) nDRef = 
  case (Map.lookup leftIdent td, Map.lookup declName td) of 
    (Just _, Just _) -> Just (ExpBinOp Mul (Ident leftIdent p) (wrapDeRef (nDRef) p $ Ident declName p) p)
    (_, _) -> Nothing

isDeclMultExpr td (PDecl name (IPtr inner) p l) nDRef = isDeclMultExpr td (PDecl name inner p l) (nDRef + 1)
isDeclMultExpr td decl _ =  Nothing 

-- Wraps the expr in a * num many times 
wrapDeRef 0 p e = e
wrapDeRef n p e = ExpUnMem PDereference (wrapDeRef (n-1) p e) p

plValToLVal :: PLValue -> LValue 
plValToLVal lval@(PLId s p) = LExpr (lValToExpr lval) p
plValToLVal lval@(PLMem mem p) = LExpr (lValToExpr lval) p
