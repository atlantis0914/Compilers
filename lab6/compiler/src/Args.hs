{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Argument and option parsing
-}
module Args (parseArgs, JobParseError(..), usage) where

import Compile.Types.Job
import System.Console.GetOpt
import Util
import Data.Maybe
import System.FilePath

data JobParseError = NoSources
                   | TooManySources
                   | GetOptError [String] deriving Show

usage :: String -> String
usage p = usageInfo p argTable

parseArgs :: Job -> [String] -> Either JobParseError Job
parseArgs initialJob args = let
  (transforms, sources, errors) = getOpt Permute argTable args
  in case errors of
       [] -> case sources of
               []        -> Left NoSources
               _ : _ : _ -> Left TooManySources
               [source]  -> Right $ ensureOut $ (foldr ($) initialJob transforms) {jobSource = source}
       _  -> Left $ GetOptError errors

argTable :: [OptDescr (Job -> Job)]
argTable = [
  Option [] ["safe"] (NoArg (setSafety True)) "Compiles with runtime checks",
  Option [] ["unsafe"] (NoArg (setSafety False)) "Compiles without runtime checks",
  Option ['O'] ["poop"] (ReqArg setOpLevel "num") "Sets the optimization level to 0",
  Option ['o'] ["out"] (ReqArg setOut "out.S") "Redirects output of the compiler to a particular target file. Will attempt to autodetect output type. - represents stdout.",
  Option ['S'] ["asm"] (NoArg (setOF Asm)) "Sets the output target to be assembly type.",
  Option ['c'] ["obj"] (NoArg (setOF Obj)) "Sets the output target to be an elf intermediate object.",
  Option ['E'] ["pretty"] (NoArg (setOF C0)) "Sets the output type to be C0 (act as a pretty printer).",
  Option ['e'] ["elf"] (NoArg (setOF ELF)) "Produces a full fledged ELF executable, ready to run.",
  Option ['l'] ["header"] (ReqArg setHeader "FILE") "Compiles against the header file specified",
  Option [] ["asmjs"] (NoArg (setOF JS)) "Sets the output type to be Asm.js, a strict subset of javascript"]

setOF :: OF -> Job -> Job
setOF outFormat j = j {jobOutFormat = outFormat}

setSafety :: Bool -> Job -> Job
setSafety b j = j {jobSafeCompilation = b}

setOpLevel :: String -> Job -> Job
setOpLevel s j = j {jobOptimization = read s}

-- setOpt :: Int -> Job -> Job
-- setOpt oLevel j = j {jobOptimization = oLevel}

extTable :: [(String, OF)]
extTable = [(".s", Asm), (".o", Obj), (".c", C0), ("", ELF),(".js", JS)]

revExtTable :: [(OF, String)]
revExtTable = map swap extTable

setOut :: FilePath -> Job -> Job
setOut out j = let
  base = j {jobOut = out}
  in case lookup (takeExtension out) extTable of
       Just f  -> setOF f base
       Nothing -> base

setHeader :: FilePath -> Job -> Job
setHeader header j = j {jobHeader = Just (header)}

ensureOut :: Job -> Job
ensureOut j = case jobOut j of
  "" -> j {jobOut = replaceExtension (jobSource j) $ objExt (jobOutFormat j)}
  _  -> j
  where objExt obj = fromJust $ lookup obj revExtTable
