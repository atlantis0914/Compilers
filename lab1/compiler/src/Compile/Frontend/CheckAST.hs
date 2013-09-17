{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Beginnings of a typechecker
-}
module Compile.Frontend.CheckAST where

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
checkAST ast@(Block stmts _) = do
  let decls = filter isDecl stmts  
  let variables = Set.fromList $ map declName decls
  assertMsgE (findDuplicate decls)
             $ length decls == Set.size variables
  rets <- fmap or $ runErrorState (mapM checkStmt stmts)
                                  (Set.empty, Set.empty, False)
  -- The state monad has state = (variables, Set.empty, returnHit) 
  assertMsgE "main does not return" rets

checkStmt (Return e _) = do
  checkExpr e
  (vars, def, retHit) <- get
  put (vars, def, True)
  return True

checkStmt (Decl i p Nothing) = do
  (vars, defined, retHit) <- get
  -- we already check the lengths - just have to add i into vars here
  put (Set.insert i vars, defined, retHit)
  return False

checkStmt (Decl i p (Just (Asgn i' m e p'))) = do
  -- At this point we've already checked for duplicate decls
  (vars, defined, retHit) <- get
  if (retHit) 
    then return True
    else do
      assertMsg "decl/assign error - idents not equal" (i == i')
      case m of
        -- Makes sure this is just an assignment operand 
        Just _ -> throwError (i ++ " used undefined at " ++ show p)
        Nothing -> return ()
      checkExpr e
      put (Set.insert i vars, Set.insert i defined, retHit)
      return False

checkStmt (Asgn i m e p) = do
  (vars, defined, retHit) <- get
  -- Ensure that the ident is already declared
  if (retHit) 
    then return True
    else do
      assertMsg (i ++ " not declared at " ++ show p) (Set.member i vars)
      case m of
        -- Ensure that an uninitialized ident doesn't do +=, -=, etc
        Just _  -> assertMsg (i ++ " used undefined at " ++ show p)
                             (Set.member i defined)
        Nothing -> return ()
      checkExpr e
      put (vars, Set.insert i defined, retHit)
      return False

checkNegExpr (ExpInt n p) = do
  assertMsg (show n ++ " too small at " ++ show p)
            (n <= (2^31))
checkNegExpr expr = checkExpr expr

checkExpr (ExpInt n p) = do
  assertMsg (show (abs n) ++ " too large at " ++ (show (2^31)) ++ show p)
            ((toInteger $ (abs n)) <= (2^31 :: Integer))
checkExpr (Ident s p) = do
  (vars, defined, retHit) <- get
  assertMsg (s ++ " used undeclared at " ++ show p) (Set.member s vars)
  assertMsg (s ++ " used undefined at " ++ show p) (Set.member s defined)

checkExpr (ExpBinOp _ e1 e2 _) = mapM_ checkExpr [e1, e2]
checkExpr (ExpUnOp Neg e _) = checkNegExpr e
checkExpr (ExpUnOp _ e _) = checkExpr e

findDuplicate xs = findDuplicate' xs Set.empty
  where findDuplicate' [] _ = error "no duplicate"
        findDuplicate' (Decl x pos _ : xs) s =
          if Set.member x s
            then x ++ " re-declared at " ++ show pos
            else findDuplicate' xs (Set.insert x s)
