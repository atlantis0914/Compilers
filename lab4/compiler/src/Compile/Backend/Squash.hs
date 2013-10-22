module Compile.Backend.Squash where 

import Compile.Types
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Data.List 

import qualified Debug.Trace as Trace

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

squash l = 
  let
    l' = Split.splitOn ("\n") l
    (_, l'') = foldl squashList ("", []) l'
  in
    foldl (++) "" (map (\x -> x ++ "\n") l'')

squashList :: (String, [String]) -> String -> (String, [String])
squashList (pS, prev) cur = 
  if (not $ obliterate pS cur)
    then (cur, prev ++ [cur])
    else (cur, prev) -- squash that shit

obliterate :: String -> String -> Bool
obliterate s1 s2 = pred1 || pred2
  where
    pred1 = (s1 == s2)
    pred2 = (bothMoves s1 s2) && (dupMoves s1 s2)

bothMoves :: String -> String -> Bool
bothMoves s1 s2 = (isInfixOf "movl" s1) && (isInfixOf "movl" s2)

getMoves :: String -> (String, String)
getMoves s = 
  let
    dl = Split.splitOn (",") (last (Split.splitOn ("movl") s))
    dl' = map trim dl
  in
    (dl' !! 0, dl' !! 1)

dupMoves s1 s2 = pred1 || pred2
  where
    (s,d) = getMoves s1
    (s',d') = getMoves s2
    pred1 = (s == s') && (d == d')
    pred2 = (s == d') && (d == s')
