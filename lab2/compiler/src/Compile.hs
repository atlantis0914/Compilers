{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Main compiler module; takes a job and compiles it
-}
module Compile
(compile
,Job(..)
,defaultJob
,OF(..)
) where

import System.FilePath
import System.Process
import System.Exit

import Control.Monad.Error

import Compile.Types
import Compile.Frontend.Parse
import Compile.Frontend.Elaborate
import Compile.Frontend.CheckInitialization
import Compile.Frontend.CheckAST
import Compile.IR.GenIR
-- import Compile.Backend.CodeGen

import LiftIOE

writer file obj = liftIOE $ writeFile file $ show obj
stringWriter file obj = liftIOE $ writeFile file $ obj

compile :: Job -> IO ()
compile job = do
  res <- runErrorT $ do -- Constructor for the error monad transformer
    ast <- parseAST $ jobSource job -- ParseAST
    elabAst <- liftEIO $ elaborate ast -- AST
    liftEIO $ checkAST elabAst
    writer (jobOut job) elabAst
--    if jobOutFormat job == C0
--      then writer (jobOut job) ast
--      else let asm = codeGen ast in
--              if jobOutFormat job == Asm
--                 then stringWriter (jobOut job) asm
--                 else do writer asmFile ast
--                         let o = if jobOutFormat job == Obj then "-c" else ""
--                         gcc o asmFile (jobOut job)
  case res of
    Left msg -> error msg
    Right () -> return ()
  where asmFile = replaceExtension (jobOut job) "s"

gcc :: String -> FilePath -> FilePath -> ErrorT String IO ()
gcc args input output = exitErrorCode $ readProcessWithExitCode
  "gcc"
  [args, input, "-o", output]
  ""
  where exitErrorCode :: IO (ExitCode, String, String) -> ErrorT String IO ()
        exitErrorCode m = do
          (exitCode, _, msg) <- lift m
          case exitCode of
            ExitSuccess   -> return ()
            ExitFailure n -> throwError $ "Error " ++ (show n) ++ "\n" ++ msg
