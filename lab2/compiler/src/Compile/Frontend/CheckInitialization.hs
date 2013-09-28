module Compile.Frontend.CheckInitialization where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad

import qualified Data.Set as Set

import Compile.Types

checkInitialization :: AST -> () 
checkInitialization (AST (Block stmts) p) = 
  let
    (_,_) = checkBlock stmts True
  in
    ()

assert :: Bool -> String -> a -> a
assert False msg x = error msg
assert _ _ x = x

verifyDecl doErr liveSet x = assert (doErr && (Set.member x liveSet)) 
                             ("Error : variable" ++ x ++ " used uninitialized")

checkStmt :: Stmt -> Bool -> (Set.Set String, Set.Set String)
checkStmt (Decl i t pos rest) doErr = 
  let
    (_, liveRest) = checkStmt rest doErr
    setI = verifyDecl doErr liveRest i $ Set.singleton(i)
  in
    (Set.empty, Set.difference liveRest setI)

checkStmt(Asgn i o e pos) doErr = (Set.singleton i, used e)

checkStmt(Expr e) doErr= (Set.empty, used e)

checkStmt(Ctrl (If e s1 s2 pos)) doErr = 
  let
    (decs1, lives1) = checkStmt s1 doErr
    (decs2, lives2) = checkStmt s2 doErr
  in
    (Set.intersection decs1 decs2, Set.union lives1 lives2)

checkStmt(Ctrl (While e s1 pos)) doErr = 
  let
    (decs1, lives1) = checkStmt s1 doErr
  in
    (Set.empty, Set.union lives1 (used e))

checkStmt(Ctrl (Return e pos)) _ = (Set.empty, used e)

checkStmt(Block stmts) _ = checkBlock stmts True
    
checkBlock :: [Stmt] -> Bool -> (Set.Set String, Set.Set String)
checkBlock [] doErr = (Set.empty, Set.empty)
checkBlock (stmt:stmts) doErr = 
  let
    (decStmt, liveStmt) = checkStmt stmt doErr
    doErr' = isReturn stmt
    (decRest, liveRest) = checkBlock stmts doErr'
  in
    if doErr'
      then (decStmt, liveStmt) 
      else (Set.union decStmt decRest, 
            Set.union liveStmt (Set.difference liveRest liveStmt))
 
   
   

isReturn (Ctrl (Return _ _)) = True
isReturn _ = False


used :: Expr -> (Set.Set String)
used (Ident id _) = Set.singleton(id)
used (ExpBinOp _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpRelOp _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpLogOp _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpPolyEq _ e1 e2 _) = Set.union (used e1) (used e2)
used (ExpUnOp _ e1 _) = used e1 
used (ExpTernary e1 e2 e3 _) = Set.union (used e1) $
                                    Set.union (used e2) (used e3)
used _ = Set.empty
    
-- 
-- maybeToSet :: Maybe (Set.Set String) -> (Set.Set String)
-- maybeToSet Nothing = Set.empty
-- maybeToSet (Just s) = s
-- 
-- setToMaybe :: (Set.Set String) -> Maybe (Set.Set String)
-- setToMaybe s 
--   | Set.null s = Nothing
--   | otherwise = Just s
-- 
-- defines :: Stmt -> Maybe (Set.Set String)
-- defines (Decl _ _ _ _) = Nothing
-- defines (Expr _) = Nothing
-- defines (Ctrl (If e s1 s2 _)) = 
--   case (defines s1, defines s2) of
--     (Just set1, Just set2) -> Just $ Set.intersection set1 set2
--     _ -> Nothing
-- defines (Ctrl (While _ _ _)) = Nothing
-- -- Return is weird - for now we implicitly return "Nothing" to represent "ALL"
-- defines (Ctrl (Return _ _)) = Nothing
-- defines (Block stmts) = setToMaybe $ foldl (\a -> 
--                                \b -> (Set.union a (maybeToSet b))) (Set.empty) (map defines stmts)
-- 
-- 
-- live :: Stmt -> Maybe (Set.Set String)


-- used :: String -> Expr -> Bool
-- used s (ExpInt _ _ _) = False
-- used s (ExpBool _ _) = False
-- used s (Ident id _) = (s == id)
-- used s (ExpBinOp _ e1 e2 _) = (used s e1 || used s e2)
-- used s (ExpRelOp _ e1 e2 _) = (used s e1 || used s e2)
-- used s (ExpLogOp _ e1 e2 _) = (used s e1 || used s e2)
-- used s (ExpPolyEq _ e1 e2 _) = (used s e1 || used s e2)
-- used s (ExpUnOp _ e1 _) = used s e1 
-- used s (ExpTernary e1 e2 e3 _) = (used s e1 || used s e2 || used s e3)
