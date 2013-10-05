{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Lift IO into Error
-}
module LiftIOE where

import Control.Monad.Error

--TODO make this actually properly catch errors like spoon
-- Lifts an IO monad type -> Error monad type 
liftIOE :: IO a -> ErrorT String IO a
liftIOE = liftIO

-- Lifts the Error monad type -> IO monad type
liftEIO :: Either String a -> ErrorT String IO a
liftEIO (Left s)  = throwError s
liftEIO (Right x) = return x
