module Compile.Backend.Interference where 

import Compile.Types
import Compile.Util.AbstractAssembly
import Compile.Util.Graph
import Compile.Backend.Registers

import Control.Monad.State
import Data.Maybe

import qualified Data.Map as Map

import qualified Debug.Trace as Trace

import System.IO.Unsafe

-- buildInterferenceGraph :: [AAsm] -> [[ALoc]] -> Graph ALoc

data Edge = Edge (ALoc, ALoc) deriving (Eq, Show, Ord)

type StateGraph = Control.Monad.State.State (Graph ALoc)

debugIO :: String -> b -> IO b
debugIO msg result = putStrLn msg >> return result

debug :: String -> a -> a
debug s r = unsafePerformIO $ debugIO s r

-- Builds an Interference Graph. Note that length aasmList must = alocList 
buildInterferenceGraph :: [AAsm] -> [[ALoc]] -> Graph ALoc
buildInterferenceGraph aasmList alocList = 
  let
    edges = getInterferenceEdges aasmList alocList
    g = newGraph 
    locs = getLocs aasmList
    g' = putVertices locs g
--    g'' = putEdges edges g'
  in
    putEdges edges g'

putVertices :: [ALoc] -> (Graph ALoc) -> (Graph ALoc)
putVertices l g = foldr addVertexGetGraph g l 

putEdges :: [Edge] -> (Graph ALoc) -> (Graph ALoc)
putEdges edgelist g = foldr putEdge g edgelist

putEdge :: Edge -> (Graph ALoc) -> (Graph ALoc)
putEdge (Edge (src,target)) graph = addEdgeSafe graph src target

-- Extracts all ALocs out of the [AAsm]
getLocs :: [AAsm] -> [ALoc]
getLocs l = getLocs' (Map.empty) l
  where getLocs' locs [] = Map.keys locs
        getLocs' locs ((AAsm {aAssign = loc}):vs) = 
          getLocs' (Map.insert (head loc) () locs) vs
        getLocs' locs ((ACtrl _):vs) = getLocs' locs vs

getInterferenceEdges :: [AAsm] -> [[ALoc]] -> [Edge]
getInterferenceEdges [] _ = []
getInterferenceEdges [AAsm {aOp = Div}] [l] = getDivConflict l
getInterferenceEdges [AAsm {aOp = Mod}] [l] = getDivConflict l
getInterferenceEdges [AAsm {aOp = RShift}] [l] = getShiftConflict l
getInterferenceEdges [AAsm {aOp = LShift}] [l] = getShiftConflict l
getInterferenceEdges [x] _ = []
getInterferenceEdges (a:a':aasm) (l:l':aloc) = 
  let
    edges = getAAsmEdges a l l'
  in
    edges ++ getInterferenceEdges (a':aasm) (l':aloc)

getAAsmEdges :: AAsm -> [ALoc] -> [ALoc] -> [Edge]
getAAsmEdges (AAsm {aAssign = assign, aOp = Div, aArgs = args}) l l' = 
  (getConflict assign l') ++ (getDivConflict l') ++ (getDivConflict l)

getAAsmEdges (AAsm {aAssign = assign, aOp = Mod, aArgs = args}) l l' = 
  (getConflict assign l') ++ (getDivConflict l') ++ (getDivConflict l)

getAAsmEdges (AAsm {aAssign = assign, aOp = RShift, aArgs = args}) l l' = 
  (getConflict assign l') ++ (getShiftConflict l') ++ (getShiftConflict l)

getAAsmEdges (AAsm {aAssign = assign, aOp = LShift, aArgs = args}) l l' = 
  (getConflict assign l') ++ (getShiftConflict l') ++ (getShiftConflict l)

getAAsmEdges (AAsm {aAssign = assign, aOp = op, aArgs = args}) l l' = getConflict assign l'

getAAsmEdges (ACtrl c) l l' = []

getConflict assign [] =  []
getConflict assign (loc:ls) = 
  let 
    to = head(assign) -- should probably make the "assigned" temp an ADT
  in
    if (loc `elem` assign) 
      then getConflict assign ls
      else [Edge (to, loc), Edge (loc, to)] ++ (getConflict assign ls)

getDivConflict :: [ALoc] -> [Edge]
getDivConflict [] = []
getDivConflict (x:xs) = [Edge (x, AReg eax_color_num), Edge (x, AReg edx_color_num)] ++ (getDivConflict xs)

getShiftConflict :: [ALoc] -> [Edge]
getShiftConflict [] = []
getShiftConflict (x:xs) = [Edge (x, AReg ecx_color_num)] ++ (getShiftConflict xs)
