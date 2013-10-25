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

checkFnList :: FnList -> (FnList, FnMap)
checkFnList fnList@(FnList gdecls pos) = 
  let 
    (tCheck, gdecls', fnMap) = checkTypeFnList fnList
    checkReturn = checkReturnFnList (FnList gdecls' pos)
    checkInitialization = checkInitializationFnList (FnList gdecls' pos)
  in
    if (tCheck && checkReturn && checkInitialization)
      then (FnList gdecls' pos, fnMap)
      else error ("Error in static check")
