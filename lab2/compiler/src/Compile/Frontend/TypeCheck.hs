module Compile.FrontEnd.TypeCheck where

import Compile.Types
import qualified Data.Map as Map

type Context = (Map.Map Ident IdentType, Bool)

checkASTTypes :: AST -> Bool
checkASTTypes

checkStmtValid :: Context -> Stmt -> Context
checkStmtValid (context@(map, valid)) (Decl declName declType expr asn) =
  let
    exists = isNothing (Map.lookup declName map)
    
  in

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
  case checkExpType context expr of Nothing -> Nothing
                                    Just t -> if t == IInt then Just IInt
                                                           else Nothing
checkExpType context (ExpTernary expr1 expr2 expr3 _) =
  case checkExpType context expr1 of
    Nothing -> Nothing
    Just t -> if t == IBool then matchType context expr2 expr3 [IInt] IInt
                            else Nothing
