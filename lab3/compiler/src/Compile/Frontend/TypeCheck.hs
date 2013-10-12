module Compile.Frontend.TypeCheck where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import Debug.Trace

--  The quad represents the 
--  1. Argument Types
--  2. Return Type
--  3. Whether this is a library decl or not. (We | these together for multiple decls)
--  4. Whether this function has already been defined
type FnMap = Map.Map String ([IdentType], IdentType, Bool, Bool)
type Context = (Map.Map String IdentType, FnMap, Bool)

-- Each function should be type checked independently 
-- foldl : (a -> b -> a) -> a -> [b] -> a
checkTypeFnList :: FnList -> Bool
checkTypeFnList (FnList gdecls pos) = 
  let
    (_,_,valid) = foldl checkGDecl (Map.empty, Map.empty, True) gdecls
  in
    valid

checkGDecl :: Context -> GDecl -> Context

-- As we don't do namespace checking here, and elaborate typedefs away
-- typedefs don't need to be very complicated. 
checkGDecl (ctx@(map, fnMap, valid)) (GTypeDef t1 t2 pos) = ctx

checkGDecl (ctx@(map, fnMap, valid)) 
           (GFDecl (FDecl {gdeclName = name,
                           gdeclArgTypes = argTypes, 
                           gdeclReturnType = returnType, 
                           gdeclIsLibrary = isLibrary}) pos) = 
  case (Map.lookup name fnMap) of 
    Just (oldArgs, oldRet, oldLib, isDeclared) -> 
      if ((lTypesEqual argTypes oldArgs) && (oldRet == returnType)) 
        then (map, Map.insert name (oldArgs, oldRet, oldLib || isLibrary, isDeclared) fnMap, valid)
        else error ("Error : function " ++ name ++ " redeclared incorrectly at " ++ (show pos))
    Nothing -> (map, Map.insert name (argTypes, returnType, isLibrary, False) fnMap, valid)

-- We must set the map to EMPTY here - each function should start type
-- checking with just its arguments as the environment. 
checkGDecl (ctx@(_, fnMap, valid)) 
      gdef@(GFDefn (FDefn {fnArgs = args,
                           fnArgTypes = argTypes}) pos) = 
  let
    map = generateIdentContext args argTypes 
  in 
    checkGFDefn (map, fnMap, valid) gdef

generateIdentContext :: [String] -> [IdentType] -> Map.Map String IdentType
generateIdentContext args argTypes = 
  foldl (\m -> \(a,at) -> Map.insert a at m) (Map.empty) $ zip args argTypes


checkGFDefn (ctx@(map, fnMap, valid)) 
            (GFDefn (FDefn {fnName = name,
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
               then checkASTTypes ctx body
               else error ("Error : function " ++ name ++ " typed incorrectly at " ++ show pos)

    Nothing -> checkASTTypes (map, 
                              Map.insert name (argTypes, retType, False, True) fnMap,
                              valid) body 

lTypesEqual :: [IdentType] -> [IdentType] -> Bool 
lTypesEqual l1 l2 = all (\(t1,t2) -> t1 == t2) $ zip l1 l2

checkASTTypes :: Context -> AST -> Context
checkASTTypes ctx (AST stmt _) = checkStmtValid ctx stmt

checkStmtValid :: Context -> Stmt -> Context
checkStmtValid (context@(map, fnMap, valid)) (Asgn name op expr pos) =
  let
    maybeExprType = checkExprType context expr
    maybeType = Map.lookup name map
    correctType = case (maybeType, maybeExprType) of
                    (Just t1, Just t2) -> t1 == t2
                    (_, _) -> False
  in
    if correctType then (map, fnMap, valid && correctType)
                   else error ("Error: Wrong type in assignment to " ++ name ++ " at " ++ show pos ++ " Expr: " ++ show expr)

checkStmtValid (context@(map, fnMap, valid)) (Decl declName declType pos asgn) =
  let
    exists = Maybe.isNothing (Map.lookup declName map)
    map' = Map.insert declName declType map
    (_,_,checkAsgn) = checkStmtValid (map', fnMap, valid) asgn
  in
    if exists then (map', fnMap, valid && exists && checkAsgn)
              else error ("Error: " ++ declName ++ " doesn't exist at " ++ show pos)

checkStmtValid (context@(map, fnMap, valid)) (Ctrl (If expr stmt1 stmt2 pos)) =
  let
    typeT = checkExprType context expr
    valid' = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
    (_, _, valid'') = checkStmtValid context stmt1
    (_, _, valid''') = checkStmtValid context stmt2
  in
    if valid' then (map, fnMap, valid' && valid'' && valid''' && valid)
              else error ("Error: If Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr ++ " type is : " ++ show typeT)

checkStmtValid (context@(map, fnMap, valid)) (Ctrl (While expr stmt pos)) =
  let
    (_, _, valid1) = checkStmtValid context stmt
    isBool = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
  in
    if isBool then (map, fnMap, valid1 && isBool && valid)
              else error ("Error: While Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr)

checkStmtValid (context@(map, fnMap, valid)) (Ctrl (Return mexpr pos)) =
  case (mexpr) of 
    Nothing -> context
    Just expr -> (
      let
        isInt = case checkExprType context expr of Nothing -> False
                                                   Just t -> t == IInt
      in
        if isInt then (map, fnMap, valid && isInt)
                 else error ("Error: Expression is not an int at " ++ show pos ++ " AST" ++ show expr))

checkStmtValid (context@(map, fnMap, valid)) (Block stmts) =
  let
    (_, _, valid') = foldl checkStmtValid context stmts
  in
    (map, fnMap, valid && valid')

checkStmtValid (context@(map, fnMap, valid)) (Expr expr) = 
  let 
    checks = case checkExprType context expr of Nothing -> False
                                                Just t -> True
  in
    (map, fnMap, valid && checks)

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
checkExprType (context@(map, _, _)) (Ident name _) = Map.lookup name map
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
checkExprType ctx@(map, fnMap, valid) (ExpFnCall fnName subExps pos) = 
  case (Map.lookup fnName fnMap) of 
    Nothing -> error ("Error : Function : " ++ fnName ++ " used undeclared at " ++ show pos)
    Just (argTypes, retType, libDecl, isDecl) -> 
      if (isDecl || libDecl) -- Then we're good, just make sure non-void ret
        then if (validateFnCall ctx argTypes subExps retType) 
               then Just retType
               else error ("Error : " ++ fnName ++ " has problems at " ++ show pos)
        else error ("Error : " ++ fnName ++ " has not been declared yet at " ++ show pos)

consumeType :: Maybe IdentType -> IdentType
consumeType Nothing = error "No type"
consumeType (Just t) = t

validateFnCall :: Context -> [IdentType] -> [Expr] -> IdentType -> Bool
validateFnCall ctx argTypes argExprs retType = 
  let
    recTypes = map (consumeType . (checkExprType ctx)) argExprs
    match = all (\(t1,t2) -> t1 == t2) $ zip argTypes recTypes
  in
    (match && (not $ retType == IVoid))

