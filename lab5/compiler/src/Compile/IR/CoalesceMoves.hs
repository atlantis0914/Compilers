module Compile.IR.CoalesceMoves where 

import Compile.Types 
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Debug.Trace as Trace

coalesceMoves :: [FnAAsm] -> [FnAAsm]
coalesceMoves fns = fns'
  where 
    fns' = map coalesceFnMoves fns

coalesceFnMoves :: FnAAsm -> FnAAsm
coalesceFnMoves a@(AAFDecl _) = a
coalesceFnMoves a@(AAFDefn al s i) = AAFDefn al' s i
  where al' = coalesceAAsmMoves al 

coalesceAAsmMoves :: [AAsm] -> [AAsm] 
coalesceAAsmMoves [] = []
coalesceAAsmMoves [x] = [x]
coalesceAAsmMoves ((x):(y):xs) = 
  if ((length sq) == 2) 
    then [x] ++ coalesceAAsmMoves (y:xs)
    else Trace.trace ("squshed") $ coalesceAAsmMoves (sq ++ xs)
  where sq = squash x y

squash a@(AAsm ([to]) Nop ([ALoc loc]))
       a'@(AAsm ([to']) Nop ([ALoc loc'])) = 
  if (to == to')
    then [a']
    else if ((to == loc') && (to' == loc))
           then [a]
           else [a, a']
squash a a' = [a, a']
