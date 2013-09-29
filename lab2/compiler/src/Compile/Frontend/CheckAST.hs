{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Beginnings of a typechecker
-}
module Compile.Frontend.CheckAST where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad

import Compile.Frontend.CheckInitialization
import Compile.Frontend.TypeCheck
import Compile.Frontend.CheckReturn

import qualified Data.Set as Set

import qualified Debug.Trace as Trace

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
checkAST ast@(AST (Block stmts) _) = do
  let tCheck = checkASTTypes ast
  let checkReturn = checkReturnAST ast
  let cRes = Trace.trace ("tCheck = " ++ show tCheck ++ 
                          " checkReturn = " ++ show checkReturn) $ checkInitialization ast
  assertMsgE "Error in checking initialization" (cRes && tCheck && checkReturn)


-- checkAST :: AST -> Either String ()
-- checkAST ast@(AST (Block stmts) _) = do
--   let decls = filter isDecl stmts
--   let variables = Set.fromList $ map declName decls
--   assertMsgE (findDuplicate decls)
--              $ length decls == Set.size variables
--   rets <- fmap or $ runErrorState (mapM checkStmt stmts)
--                                   (Set.empty, Set.empty, False)
--   -- The state monad has state = (variables, Set.empty, returnHit)
--   assertMsgE "main does not return" rets

-- checkStmt (Ctrl (Return e _)) = do
--   checkExpr e
--   (vars, def, retHit) <- get
--   put (vars, def, True)
--   return True
-- 
-- checkStmt (Decl i t p Nothing) = do
--   (vars, defined, retHit) <- get
--   -- we already check the lengths - just have to add i into vars here
--   put (Set.insert i vars, defined, retHit)
--   return False
-- 
-- checkStmt (Decl i t p (Just (Asgn i' m e p'))) = do
--   -- At this point we've already checked for duplicate decls
--   (vars, defined, retHit) <- get
--   if (retHit)
--     then do put (Set.insert i vars, defined, retHit)
--             checkExpr e
--             return True
--     else do
--       assertMsg "decl/assign error - idents not equal" (i == i')
--       case m of
--         -- Makes sure this is just an assignment operand
--         Just _ -> throwError (i ++ " used undefined 1 at " ++ show p)
--         Nothing -> return ()
--       checkExpr e
--       put (Set.insert i vars, Set.insert i defined, retHit)
--       return False
-- 
-- checkStmt (Asgn i m e p) = do
--   (vars, defined, retHit) <- get
--   -- Ensure that the ident is already declared
--   assertMsg (i ++ " not declared at " ++ show p) (Set.member i vars)
--   if (retHit)
--     then do
--        checkExpr e
--        return True
--     else do
--       case m of
--         -- Ensure that an uninitialized ident doesn't do +=, -=, etc
--         Just _  -> assertMsg (i ++ " used undefined 2 at " ++ show p)
--                              (Set.member i defined)
--         Nothing -> return ()
--       checkExpr e
--       put (vars, Set.insert i defined, retHit)
--       return False
-- 
-- 
-- checkExpr (ExpInt n p Dec) = do
--   assertMsg (show n ++ " too large at " ++ show p)
--             (n <= (2^31))
-- 
-- checkExpr (ExpInt n p Hex) = do
--   assertMsg (show n ++ " too large at " ++ show p)
--             (n <= (2^32))
-- 
-- checkExpr (Ident s p) = do
--   (vars, defined, retHit) <- get
--   assertMsg (s ++ " used undeclared at " ++ show p) (Set.member s vars)
--   if (retHit)
--     then return ()
--     else do
--             assertMsg (s ++ " used undefined at " ++ show p) (Set.member s defined)
--             return ()
-- 
-- checkExpr (ExpBinOp op e1 e2 p) = do checkOp op p
--                                      mapM_ checkExpr [e1, e2]
-- 
-- checkExpr (ExpUnOp op e p) = do checkOp op p
--                                 checkExpr e
-- 
-- checkOp op p = assertMsg ("Saw unqualified -- at " ++ show p) (not $ op == Decr)
-- 
-- findDuplicate xs = findDuplicate' xs Set.empty
--   where findDuplicate' [] _ = error "no duplicate"
--         findDuplicate' (Decl x t pos _ : xs) s =
--           if Set.member x s
--             then x ++ " re-declared at " ++ show pos
--             else findDuplicate' xs (Set.insert x s)
