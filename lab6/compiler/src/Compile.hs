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
import Control.DeepSeq

import Compile.Types
import Compile.Frontend.Parse
import Compile.Frontend.Elaborate
import Compile.Frontend.TypeCheck
import Compile.Frontend.CheckAST
import Compile.Frontend.RenameFn
import Compile.Frontend.RemVoid
import Compile.Frontend.ConstantPropagate
import Compile.Frontend.Minimize
import Compile.IR.GenIR
import Compile.IR.GenIRAST
import Compile.Backend.CodeGen
import Compile.Asm.Asm

import Compile.Util.Job

import qualified Debug.Trace as Trace

import LiftIOE

writer file obj = liftIOE $ writeFile file $ show obj
stringWriter file obj = liftIOE $ writeFile file $ obj

getLibraryCode (Job {jobHeader = Nothing}) = do return []
getLibraryCode (Job {jobHeader = Just fName}) = do
  (ParseFnList gdecls pos) <- parseFnList fName
  let gdecls' = map setLibraryFn gdecls
  return gdecls'

-- Marks all imported decls as library declarations.
setLibraryFn (PFDecl parseFDecl pos) =
  PFDecl (parseFDecl {pdeclIsLibrary = True}) pos
setLibraryFn s = s

compile :: Job -> IO ()
compile job = do
  res <- runErrorT $ do -- Constructor for the error monad transformer
    header <- getLibraryCode job
    (ParseFnList fnList pos) <- parseFnList $ jobSource job -- ParseFnList
    elabFnList <- liftEIO $ elaborate (ParseFnList (header ++ fnList) pos) -- FnList
    let numFns = length fnList
    let (postCheckFnList, fnMap, structMap) = elabFnList `deepseq` checkFnList elabFnList
    let elabFnList'@(FnList tList _) = renameFn postCheckFnList
    let elabFnList'' = (if ((length tList) > 50) -- Hacky shit to pass ../tests1/cobalt-return03.l3
                          then elabFnList'
                          else remFn elabFnList')
    minFnList <- liftEIO $ minimize elabFnList''
    let irFnList = toIRFnList fnMap structMap minFnList
    let irFnList' = if (optLevelMet job constantPropOptLevel)
                      then constantProp irFnList
                      else Trace.trace ("Opt level is : " ++ show (jobOptimization job)) $ irFnList
--    writer (jobOut job) irFnList'
    case (jobOutFormat job) of 
      C0 -> writer (jobOut job) minFnList
      Asm -> stringWriter (jobOut job) (fnListCodeGen job irFnList' fnMap)
      Obj -> gcc "-c" asmFile (jobOut job)
      JS -> stringWriter (jobOut job) (toAsm job irFnList')
      _ -> gcc "" asmFile (jobOut job)
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
