{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines a compiler phase or job
-}
module Compile.Types.Job where

data Job = Job
  { jobOut       :: FilePath
  , jobSource    :: FilePath
  , jobOutFormat :: OF
  , jobHeader    :: Maybe FilePath
  }

data OF = C0
        | Asm
        | Obj
        | ELF
        | Header String deriving Eq

defaultJob :: Job
defaultJob = Job {jobOut       = "",
                  jobSource    = "",
                  jobOutFormat = ELF,
                  jobHeader    = Nothing}
