{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Lift IO into Error
-}
module LiftIOE where

import Control.Monad.Error

--TODO make this actually properly catch errors like spoon
liftIOE :: IO a -> ErrorT String IO a
liftIOE = liftIO

liftEIO :: Either String a -> ErrorT String IO a
liftEIO (Left s)  = throwError s
liftEIO (Right x) = return x
