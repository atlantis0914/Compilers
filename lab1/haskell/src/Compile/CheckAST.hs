{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Beginnings of a typechecker
-}
module Compile.CheckAST where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad

import qualified Data.Set as Set

import Compile.Types

-- Note to the student
-- When your checker gets larger, you may wish to formalize the state
-- a little more.

-- This is hacky and not designed to scale.

runErrorState :: ErrorT String (State s) a -> s -> Either String a
runErrorState m s = evalState (runErrorT m) s

assertMsg :: (Monad m) => String -> Bool -> ErrorT String m ()
assertMsg s True  = return ()
assertMsg s False = throwError s

assertMsgE :: String -> Bool -> Either String ()
assertMsgE s True  = Right ()
assertMsgE s False = Left s

checkAST :: AST -> Either String ()
checkAST ast@(Block decls stmts _) = do
  let variables = Set.fromList $ map declName decls
  assertMsgE (findDuplicate decls)
             $ (length decls) == (Set.size variables)
  rets <- fmap or $ runErrorState (mapM checkStmt stmts) $
                                  (variables, Set.empty)
  assertMsgE "main does not return" rets

checkStmt (Return e _) = do
  checkExpr e
  return True
checkStmt (Asgn i m e p) = do
  (vars, defined) <- get
  assertMsg (i ++ " not declared at " ++ (show p)) (Set.member i vars)
  case m of
    Just _  -> assertMsg (i ++ " used undefined at " ++ (show p))
                         (Set.member i defined)
    Nothing -> return ()
  checkExpr e
  put (vars, Set.insert i defined)
  return False

checkExpr (ExpInt n p) =
  assertMsg ((show n) ++ " too large at " ++ (show p))
            (n < 2^32)
checkExpr (Ident s p) = do
  (vars, defined) <- get
  assertMsg (s ++ " used undeclared at " ++ (show p)) (Set.member s vars)
  assertMsg (s ++ " used undefined at " ++ (show p)) (Set.member s defined)
checkExpr (ExpBinOp _ e1 e2 _) = mapM_ checkExpr [e1, e2]
checkExpr (ExpUnOp _ e _) = checkExpr e

findDuplicate xs = findDuplicate' xs Set.empty
  where findDuplicate' [] _ = error "no duplicate"
        findDuplicate' (Decl x pos : xs) s =
          if Set.member x s
            then x ++ " re-declared at " ++ (show pos)
            else findDuplicate' xs (Set.insert x s)
