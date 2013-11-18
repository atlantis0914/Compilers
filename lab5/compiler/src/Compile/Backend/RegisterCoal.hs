module Compile.Backend.RegisterCoal where 

import Compile.Types
import Compile.Util.Graph
import Compile.Backend.Coloring 
import Compile.Backend.Registers
import Compile.Util.Color
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State

import qualified Debug.Trace as Trace

type CoalesceState = (Graph ALoc, ColoringMap, [AAsm], [AAsm])
-- Nah jk, the first [AAsm] is the stuff left to greedily process
-- the second one is what we've already consumned. We keep this around
-- in state so that we can propagate coalesces both forwards and backwards 
-- in time (this is n^2 in the worst case, but I'm more worried about the 
-- generated code's correctness). 

type RCState = State CoalesceState

prohibList = [Color 0, Color 1, Color 2]

registerCoalesce :: [AAsm] -> ColoringMap -> Graph ALoc -> ([AAsm], ColoringMap)
registerCoalesce aasm cmap ig = 
  let
    (ig', cmap', rem, aasm') = execState coalesceAAsm (ig, cmap, aasm, [])
  in
    (aasm', cmap')

coalesceAAsm :: RCState ()
coalesceAAsm = do
  (ig, cmap, proc, fin) <- get
  consumeAAsmL proc 

consumeAAsmL :: [AAsm] -> RCState ()
consumeAAsmL [] = do 
  return ()

consumeAAsmL (x:xs) = do 
  (ig, cmap, _, fin) <- get
  put (ig, cmap, xs, fin)
  consumeAAsm x 

consumeAAsm :: AAsm -> RCState ()
consumeAAsm aasm@(AAsm [dest] _ [(ALoc src)]) = do
  (ig, cmap, proc, fin) <- get
  let c1 = isEdge ig dest src 
  let c2 = sameColor cmap dest src
  let c3 = badLoc dest || badLoc src

  let nh1 = getNeighborsAsSet ig src
  let nh2 = getNeighborsAsSet ig dest
  let nghs = Set.union nh1 nh2
  let colors = map (\s -> cmap Map.! s) (Set.toList nghs)
  let lowest@(Color low) = getLowestColor $ nub (colors ++ prohibList)
  let c4 = ((low < 7) || (low > 10)) -- coalesce into callee save only atm
  if (c1 || c2 || c3 || c4) 
    then (do 
      put (ig, cmap, proc, fin ++ [aasm])
      coalesceAAsm
      return ())
    else coalesceMove aasm lowest

consumeAAsm aasm = do
  (ig, cmap, proc, fin) <- get 
  put (ig, cmap, proc, fin ++ [aasm])
  coalesceAAsm
  return ()

coalesceMove :: AAsm -> Color -> RCState ()
coalesceMove aasm@(AAsm [dest] o [(ALoc src)]) col = do
  (ig@(Graph gMap), cmap, proc, fin) <- get
  -- Update colors of src, dest
  let cmap' = Map.insert src col cmap
  let cmap'' = Map.insert dest col cmap'
  let nh1 = getNeighborsAsSet ig src
  let nh2 = getNeighborsAsSet ig dest
  -- delete src from ig, add src's edges to dest. 
  let ig' = Graph (Map.delete src gMap)
  -- update all nodes with src as adj to point to dest
  let ig'' = updateVertexPtrs ig' src dest 
  -- add edges from dest -> edges that src had 
  let ig''' = foldl (addEdgeFoldPat dest) ig'' (Set.toList nh1)
  let aasm' = AAsm [dest] o [(ALoc dest)] 
  put (ig''', cmap'', proc, fin ++ [aasm'])
  updateProcFin src dest 
  coalesceAAsm
  return ()

updateProcFin :: ALoc -> ALoc -> RCState ()
updateProcFin old new = do
  (ig, cmap, proc, fin) <- get
  let proc' = map (replaceLocs old new) proc
  let fin' = map (replaceLocs old new) fin
  put (ig, cmap, proc', fin')
  return ()

replaceLocs :: ALoc -> ALoc -> AAsm -> AAsm
replaceLocs old new (AAsm dests op srcs) = 
  AAsm (map (lookupLoc old new) dests) op (map (lookupVal old new) srcs)

replaceLocs old new (AFnCall s l1 l2list l3list) = 
  AFnCall s (lookupLoc old new l1)
             (map (lookupLoc old new) l2list)
             (map (lookupLoc old new) l3list)

replaceLocs old new (ACtrl (ARet (ALoc loc))) = 
  ACtrl (ARet (ALoc (lookupLoc old new loc)))

replaceLocs old new (ACtrl (AIf (ALoc loc) i s opt)) = 
  ACtrl (AIf (ALoc $ lookupLoc old new loc) i s opt)

replaceLocs _ _ a = a

lookupVal :: ALoc -> ALoc -> AVal -> AVal
lookupVal old new (ALoc loc) = (ALoc (lookupLoc old new loc))
lookupVal _ _ v = v

lookupLoc :: ALoc -> ALoc -> ALoc -> ALoc
lookupLoc old new v1@(ATemp tn tb) =
  if (old == v1)
    then new
    else v1
--               Just (v1', _) -> v1'
--               Nothing -> case (Map.lookup (ATemp tn (not tb)) coalMap) of 
--                            Just (v1', _) -> v1'
--                            Nothing -> v1

lookupLoc old new v1@(APtr l1 (Just l2) i1 i2 b) = 
  APtr (lookupLoc old new l1) (Just (lookupLoc old new l2))
    i1 i2 b

lookupLoc old new v1@(APtr l1 Nothing i1 i2 b) = 
  APtr (lookupLoc old new l1) Nothing
    i1 i2 b

lookupLoc _ _ loc = loc
     
badLoc :: ALoc -> Bool
badLoc (ATemp _ _) = False
badLoc _ = True
 
sameColor :: ColoringMap -> ALoc -> ALoc -> Bool
sameColor coloring v1 v2 = 
  case (Map.lookup v1 coloring, Map.lookup v2 coloring) of
    (Just (Color c1), Just (Color c2)) -> c1 == c2
    _ -> False
