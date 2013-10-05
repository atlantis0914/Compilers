module Compile.Frontend.TypeCheck where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import Debug.Trace

type Context = (Map.Map String IdentType, Bool)

checkASTTypes :: AST -> Bool
checkASTTypes (AST stmt _) =
  let
    (_, valid) = checkStmtValid (Map.empty, True) stmt
  in
    valid

checkStmtValid :: Context -> Stmt -> Context
checkStmtValid (context@(map, valid)) (Asgn name op expr pos) =
  let
    maybeExprType = checkExprType context expr
    maybeType = Map.lookup name map
    correctType = case (maybeType, maybeExprType) of
                    (Just t1, Just t2) -> t1 == t2
                    (_, _) -> False
  in
    if correctType then (map, valid && correctType)
                   else error ("Error: Wrong type in assignment to " ++ name ++ " at " ++ show pos ++ " Expr: " ++ show expr)

checkStmtValid (context@(map, valid)) (Decl declName declType pos asgn) =
  let
    exists = Maybe.isNothing (Map.lookup declName map)
    map' = Map.insert declName declType map
    checkAsgn = Tuple.snd $ checkStmtValid (map', valid) asgn
  in
    if exists then (map', valid && exists && checkAsgn)
              else error ("Error: " ++ declName ++ " doesn't exist at " ++ show pos)

checkStmtValid (context@(map, valid)) (Ctrl (If expr stmt1 stmt2 pos)) =
  let
    valid' = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
    (_, valid'') = checkStmtValid context stmt1
    (_, valid''') = checkStmtValid context stmt2
  in
    if valid' then (map, valid' && valid'' && valid''' && valid)
              else error ("Error: If Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr)

checkStmtValid (context@(map, valid)) (Ctrl (While expr stmt pos)) =
  let
    (_, valid1) = checkStmtValid context stmt
    isBool = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
  in
    if isBool then (map, valid1 && isBool && valid)
              else error ("Error: While Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr)

checkStmtValid (context@(map, valid)) (Ctrl (Return expr pos)) =
  let
    isInt = case checkExprType context expr of Nothing -> False
                                               Just t -> t == IInt
  in
    if isInt then (map, valid && isInt)
             else error ("Error: Expression is not an int at " ++ show pos ++ " AST" ++ show expr)

checkStmtValid (context@(map, valid)) (Block stmts) =
  let
    (_, valid') = foldl checkStmtValid context stmts
  in
    (map, valid && valid')

checkStmtValid (context@(map, valid)) (Expr expr) = 
  let 
    checks = case checkExprType context expr of Nothing -> False
                                                Just t -> True
  in
    (map, valid && checks)

checkStmtValid context SNop =
  context

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
checkExprType (context@(map, _)) (Ident name _) = Map.lookup name map
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

