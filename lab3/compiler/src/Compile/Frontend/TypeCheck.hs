module Compile.Frontend.TypeCheck where

import Compile.Types
import Compile.Util.IdentTypeUtil 

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import Debug.Trace


--  The quad in FnMap represents the 
--  1. Argument Types
--  2. Return Type
--  3. Whether this is a library decl or not. (We | these together for multiple decls)
--  4. Whether this function has already been defined
type FnMap = Map.Map String ([IdentType], IdentType, Bool, Bool) -- Map of global fn defines 
type DeclMap = Map.Map String Bool  -- Map of what decls are currently in scope
type IdentMap = Map.Map String IdentType -- Map from idents -> types
type TDMap = Map.Map IdentType IdentType -- Typedef Map

type Context = (IdentMap, FnMap, DeclMap, TDMap, Bool)

-- Each function should be type checked independently 
-- foldl : (a -> b -> a) -> a -> [b] -> a
checkTypeFnList :: FnList -> Bool
checkTypeFnList (FnList gdecls pos) = 
  let
    initFnMap = foldl genFnMap (Map.empty) gdecls
    (_, endMap, _, _, valid) = foldl checkGDecl (Map.empty, initFnMap, Map.empty, baseIdentTypeMap, True) gdecls
  in  
    case (Map.lookup "main" endMap) of
      Nothing -> error ("Error : int main() must be declared.")
      Just (argTypes, retType, lDecl, defn) -> (
        if ((length argTypes == 0) && (retType == IInt)) 
          then valid
          else error ("Error : int main() must be the right type"))

lTypesEqual :: [IdentType] -> [IdentType] -> Bool 
lTypesEqual l1 l2 = all (\(t1,t2) -> t1 == t2) $ zip l1 l2

-- Used to create an initial function map. Also checks basic declaration
-- and redeclaration properties over the top-level program. 
genFnMap :: FnMap -> GDecl -> FnMap
genFnMap fnMap (GTypeDef _ _ _) = fnMap
genFnMap fnMap (GFDecl (FDecl {gdeclName = name,
               gdeclArgTypes = argTypes, 
               gdeclReturnType = returnType, 
               gdeclIsLibrary = isLibrary}) pos) = 
  case (Map.lookup name fnMap) of 
    Just (oldArgs, oldRet, oldLib, isDeclared) -> 
      if ((lTypesEqual argTypes oldArgs) && (oldRet == returnType)) 
        then Map.insert name (oldArgs, oldRet, oldLib || isLibrary, isDeclared) fnMap
        else error ("Error : function " ++ name ++ " redeclared incorrectly at " ++ (show pos))
    Nothing -> Map.insert name (argTypes, returnType, isLibrary, False) fnMap

genFnMap fnMap (GFDefn (FDefn {fnName = name,
                               fnArgs = args,
                               fnArgTypes = argTypes,
                               fnReturnType = retType,
                               fnBody = body}) pos) = 
  case (Map.lookup name fnMap) of 
    Just (oldArgs, oldRet, oldLib, isDeclared) -> 
      if (isDeclared || oldLib) 
        -- One error message, two birds
        then error ("Error : function " ++ name ++ " redefined or library at " ++ show pos)
        else if ((lTypesEqual argTypes oldArgs) && (oldRet == retType)) 
               then (Map.insert name (argTypes, retType, oldLib, True) fnMap)
               else error ("Error : function " ++ name ++ " typed incorrectly at " ++ show pos)
    Nothing -> (Map.insert name (argTypes, retType, False, True) fnMap)


checkGDecl :: Context -> GDecl -> Context
-- It's our responsibility here to ensure that the name being type-def'd
-- isn't already used as a function name. 
checkGDecl (ctx@(map, fnMap, dMap, tdMap, valid)) (GTypeDef t1 t2 pos) = 
  case t2 of 
    (ITypeDef s2) -> if (Map.lookup s2 fnMap == Nothing)
                       then (map, fnMap, dMap, Map.insert t2 t1 tdMap, valid)
                       else error ("Error : type name " ++ s2 ++ 
                                    " already used as fn name")
    _ -> error ("Typedef to concrete type at " ++ show pos)

checkGDecl (ctx@(map, fnMap, dMap, tdMap, valid)) 
           (GFDecl (FDecl {gdeclName = name,
                           gdeclArgTypes = argTypes, 
                           gdeclReturnType = returnType, 
                           gdeclIsLibrary = isLibrary}) pos) = 
  (map, fnMap, Map.insert name True dMap, tdMap, valid)
--   case (Map.lookup name fnMap) of 
--     Just (oldArgs, oldRet, oldLib, isDeclared) -> 
--       if ((lTypesEqual argTypes oldArgs) && (oldRet == returnType)) 
--         then (map, Map.insert name (oldArgs, oldRet, oldLib || isLibrary, isDeclared) fnMap, valid)
--         else error ("Error : function " ++ name ++ " redeclared incorrectly at " ++ (show pos))
--     Nothing -> (map, Map.insert name (argTypes, returnType, isLibrary, False) fnMap, valid)

-- We must set the map to EMPTY here - each function should start type
-- checking with just its arguments as the environment. 
checkGDecl (ctx@(_, fnMap, dMap, tdMap, valid)) 
      gdef@(GFDefn (FDefn {fnArgs = args,
                           fnName = name,
                           fnArgTypes = argTypes,
                           fnBody = body}) pos) = 
  let
    idMap = generateIdentContext args argTypes 
  in 
    checkASTTypes (idMap, fnMap, Map.insert name True dMap, tdMap, valid) body 
--    checkGFDefn (map, fnMap, valid) gdef

generateIdentContext :: [String] -> [IdentType] -> Map.Map String IdentType
generateIdentContext args argTypes = 
  foldl (\m -> \(a,at) -> Map.insert a at m) (Map.empty) $ zip args argTypes

checkGFDefn (ctx@(map, fnMap, dMap, tdMap, valid)) 
            (GFDefn (FDefn {fnName = name,
                            fnArgs = args,
                            fnArgTypes = argTypes,
                            fnReturnType = retType,
                            fnBody = body}) pos) = 
  checkASTTypes (map, fnMap, Map.insert name True dMap, tdMap, valid) body

checkASTTypes :: Context -> AST -> Context
checkASTTypes ctx (AST stmt _) = checkStmtValid ctx stmt

checkStmtValid :: Context -> Stmt -> Context
checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Asgn name op expr pos) =
  let
    maybeExprType = checkExprType context expr
    maybeType = Map.lookup name map
    correctType = case (maybeType, maybeExprType) of
                    (Just t1, Just t2) -> t1 == t2
                    (_, _) -> False
  in
    if correctType then (map, fnMap, dMap, tdMap, valid && correctType)
                   else error ("Error: Wrong type in assignment to " ++ name ++ " at " ++ show pos ++ " Expr: " ++ show expr)

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Decl declName declType pos asgn) =
  let
    exists = Maybe.isNothing (Map.lookup declName map)
    map' = Map.insert declName declType map
    (_,_,_,_,checkAsgn) = checkStmtValid (map', fnMap, dMap, tdMap, valid) asgn
  in
    if exists then (map', fnMap, dMap, tdMap, valid && exists && checkAsgn)
              else error ("Error: " ++ declName ++ " doesn't exist at " ++ show pos)

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Ctrl (If expr stmt1 stmt2 pos)) =
  let
    typeT = checkExprType context expr
    valid' = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
    (_, _, _, _, valid'') = checkStmtValid context stmt1
    (_, _, _, _, valid''') = checkStmtValid context stmt2
  in
    if valid' then (map, fnMap, dMap, tdMap, valid' && valid'' && valid''' && valid)
              else error ("Error: If Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr ++ " type is : " ++ show typeT)

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Ctrl (While expr stmt pos)) =
  let
    (_, _, _, _, valid1) = checkStmtValid context stmt
    isBool = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
  in
    if isBool then (map, fnMap, dMap, tdMap, valid1 && isBool && valid)
              else error ("Error: While Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr)

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Ctrl (Return mexpr pos)) =
  case (mexpr) of 
    Nothing -> context
    Just expr -> (
      let
        isInt = case checkExprType context expr of Nothing -> False
                                                   Just t -> t == IInt
      in
        if isInt then (map, fnMap, dMap, tdMap, valid && isInt)
                 else error ("Error: Expression is not an int at " ++ show pos ++ " AST" ++ show expr))

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Block stmts) =
  let
    (_, _, _, _, valid') = foldl checkStmtValid context stmts
  in
    (map, fnMap, dMap, tdMap, valid && valid')

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Expr fn@(ExpFnCall fnName subExps pos)) = 
  let
    checks = case checkFnCall context fn True of Nothing -> False 
                                                 Just t -> True
  in
    (map, fnMap, dMap, tdMap, valid && checks)

checkStmtValid (context@(map, fnMap, dMap, tdMap, valid)) (Expr expr) = 
  let 
    checks = case checkExprType context expr of Nothing -> False
                                                Just t -> True
  in
    (map, fnMap, dMap, tdMap, valid && checks)

checkStmtValid context SNop = context

matchType :: Context -> Expr -> Expr -> [IdentType] -> IdentType -> (Maybe IdentType)
matchType context expr1 expr2 expect result =
  let
    type1 = checkExprType context expr1
    type2 = checkExprType context expr2
  in
    case (type1, type2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just t1, Just t2) -> if t1 == t2 && t1 `elem` expect then Just result
                                                            else Nothing

typeEq :: Context -> Expr -> Expr -> [IdentType] -> (Maybe IdentType)
typeEq context expr1 expr2 expect =
  let
    type1 = checkExprType context expr1
    type2 = checkExprType context expr2
  in
    case (type1, type2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just t1, Just t2) -> if t1 == t2 && t1 `elem` expect then Just t1
                                                            else Nothing

checkExprIsType :: Maybe IdentType -> IdentType -> Maybe IdentType
checkExprIsType maybeType t =
  case maybeType of Nothing -> Nothing
                    Just t' -> if t' == t then Just t
                                          else Nothing

checkExprType :: Context -> Expr -> Maybe IdentType
checkExprType _ (ExpInt _ _ _) = Just IInt
checkExprType _ (ExpBool _ _) = Just IBool
checkExprType (context@(map, _, _, _, _)) (Ident name _) = Map.lookup name map
checkExprType context (ExpBinOp _ expr1 expr2 _) =
  matchType context expr1 expr2 [IInt] IInt
checkExprType context (ExpRelOp _ expr1 expr2 _) =
  matchType context expr1 expr2 [IInt] IBool
checkExprType context (ExpLogOp _ expr1 expr2 _) =
  matchType context expr1 expr2 [IBool] IBool
checkExprType context (ExpPolyEq _ expr1 expr2 _) =
  matchType context expr1 expr2 [IBool, IInt] IBool
checkExprType context (ExpUnOp Neg expr _) =
  checkExprIsType (checkExprType context expr) IInt
checkExprType context (ExpUnOp BitwiseNot expr _) =
  checkExprIsType (checkExprType context expr) IInt
checkExprType context (ExpUnOp LogicalNot expr _) =
  checkExprIsType (checkExprType context expr) IBool
checkExprType context (ExpTernary expr1 expr2 expr3 _) =
  case checkExprType context expr1 of
    Nothing -> Nothing
    Just t -> if t == IBool then typeEq context expr2 expr3 [IInt, IBool]
                            else Nothing
checkExprType ctx@(map, fnMap, dMap, tdMap, valid) call@(ExpFnCall fnName subExps pos) = 
  checkFnCall ctx call False

checkFnCall ctx@(map, fnMap, dMap, tdMap, valid) (ExpFnCall fnName subExps pos) canBeVoid = 
  case (Map.lookup fnName dMap, Map.lookup fnName fnMap) of 
    (Nothing, _) -> error ("Error : Function : " ++ fnName ++ " used undeclared at " ++ show pos)
    (_, Just (argTypes, retType, libDecl, isDecl)) -> 
      if (isDecl || libDecl) -- Then we're good, just make sure non-void ret
        then if (validateFnCall ctx fnName argTypes subExps retType canBeVoid pos) 
               then Just retType
               else error ("Error : " ++ fnName ++ " invocation has problems at " ++ show pos)
        else error ("Error : " ++ fnName ++ " has not been declared yet at " ++ show pos)

consumeType :: Maybe IdentType -> IdentType
consumeType Nothing = error "No type"
consumeType (Just t) = t

-- validateFnCall :: Context -> String -> [IdentType] -> [Expr] -> IdentType -> Bool
validateFnCall (ctx@(idMap, _, _, tdMap, _)) fnName argTypes argExprs retType canBeVoid pos = 
  let
    recTypes = map (consumeType . (checkExprType ctx)) argExprs
    match = all (\(t1,t2) -> ((tdMap Map.! t1) == (tdMap Map.! t2))) $ zip argTypes recTypes
    isShadowed = (Map.lookup fnName idMap == Nothing) 
  in
    if (not match)
      then error ("Error : Function types don't match on invocation of : " ++ fnName ++ " at " ++ show pos)
      else (
    if ((retType == IVoid) && (not canBeVoid))
      then error ("Error : Cannot use invoke void function : " ++ fnName ++ " at " ++ show pos)
      else (
    if (not $ length argTypes == length recTypes)
      then error ("Error : problem with parameters during invocation of : " ++ fnName ++ " at " ++ show pos)
      else (
    if (not isShadowed) 
      then error ("Error : function name, " ++ fnName ++ ", shadowed at " ++ show pos)
      else True)))
