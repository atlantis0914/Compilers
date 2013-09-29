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
checkStmtValid (context@(map, valid)) (Asgn name op expr _) =
  let
    maybeExprType = checkExprType context expr
    maybeType = Map.lookup name map
    correctType = case (maybeType, maybeExprType) of
                    (Just t1, Just t2) -> t1 == t2
                    (_, _) -> False
  in
    (map, valid && correctType)

checkStmtValid (context@(map, valid)) (Decl declName declType _ asgn) =
  let
    exists = Maybe.isNothing (Map.lookup declName map)
    map' = Map.insert declName declType map
    checkAsgn = Tuple.snd $ checkStmtValid (map', valid) asgn
  in
    (map', valid && exists && checkAsgn)

checkStmtValid (context@(map, valid)) (Ctrl (If expr stmt1 stmt2 _)) =
  let
    valid' = case checkExprType context expr of Nothing -> False
                                                Just t -> t == IBool
    (_, valid'') = checkStmtValid context stmt1
    (_, valid''') = checkStmtValid context stmt2
  in
    (map, valid' && valid'' && valid''' && valid)

checkStmtValid (context@(map, valid)) (Ctrl (While exp stmt _)) =
  let
    (_, valid1) = checkStmtValid context stmt
    isBool = case checkExprType context exp of Nothing -> False
                                               Just t -> t == IBool
  in
    (map, valid1 && isBool && valid)

checkStmtValid (context@(map, valid)) (Ctrl (Return expr _)) =
  let
    isInt = case checkExprType context expr of Nothing -> False
                                               Just t -> t == IInt
  in
    (map, valid && isInt)

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
    (map, checks)

-- checkStmtValid (context@(map, valid)) s = trace("S is = " ++ show s) $ (map, True)


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
checkExprType context (ExpUnOp _ expr _) =
  case checkExprType context expr of Nothing -> Nothing
                                     Just t -> if t == IInt then Just IInt
                                                            else Nothing
checkExprType context (ExpTernary expr1 expr2 expr3 _) =
  case checkExprType context expr1 of
    Nothing -> Nothing
    Just t -> if t == IBool then matchType context expr2 expr3 [IInt] IInt
                            else Nothing
