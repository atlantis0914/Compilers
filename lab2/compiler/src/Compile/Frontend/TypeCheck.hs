module Compile.FrontEnd.TypeCheck where

import Compile.Types
import qualified Data.Map as Map

type Context = (Map.Map Ident IdentType, Bool)

checkASTTypes :: AST -> Bool
checkASTTypes

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
    exists = isNothing (Map.lookup declName map)
    map' = insert declName declType map
    checkAsgn = case asgn of Nothing -> True
                             Just asgn' -> checkStmtValid context asgn'
  in
    (map', valid && exists && checkAsgn)
checkStmtValid (context@(map, valid)) (Ctrl (If exp stmt1 stmt2 _)) =
  let
    (_, valid1) = checkStmtValid context stmt1
    (_, valid2) = checkStmtValid context stmt2
  in
    (map, valid1 && valid2 && valid)
checkStmtValid (context@(map, valid)) (Ctrl (While exp stmt _)) =
  let
    (_, valid1) = checkStmtValid context stmt
  in
    (map, valid1 && valid)
checkStmtValid (context@(map, valid)) (Ctrl (Return exp _)) =
  let
    isInt = case checkExprType context expr of Nothing -> False
                                               Just t -> t == IInt
  in
    (map, valid && isInt)
checkStmtValid (context@(map, valid)) (Block stmts) =
  fold checkStmtValid stmts


matchType :: Context -> Expr -> Expr -> [IdentType] -> IdentType -> (Maybe IdentType)
matchType context expr1 expr2 expect result =
  type1 = checkExprType context expr1
  type2 = checkExprType context expr2
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
checkExprType context (ExpPolyEq _ expr _) =
  case checkExprType context expr of Nothing -> Nothing
                                    Just t -> if t == IInt then Just IInt
                                                           else Nothing
checkExprType context (ExpTernary expr1 expr2 expr3 _) =
  case checkExprType context expr1 of
    Nothing -> Nothing
    Just t -> if t == IBool then matchType context expr2 expr3 [IInt] IInt
                            else Nothing
