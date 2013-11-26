module Compile.IR.CoalesceLabel where 

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Debug.Trace as Trace

type LabelMap = Map.Map Int Int

coalesceLabel :: [FnAAsm] -> [FnAAsm]
coalesceLabel fns = fns'
  where 
    lTranslations = map labelsForFn fns
    fnZip = zip fns lTranslations
    fns' = map coalesceFnAAsm fnZip
--   where (_, fns') = foldl coalesceFnAAsm (Map.empty, []) fns

coalesceFnAAsm :: (FnAAsm, LabelMap) -> FnAAsm
coalesceFnAAsm (a@(AAFDecl _), m) = a
coalesceFnAAsm (a@(AAFDefn al s i), m) = AAFDefn al' s i
  where al' = translateBadLabels al m 

translateBadLabels :: [AAsm] -> LabelMap -> [AAsm]
translateBadLabels al l = map (translateLabel l) al
  where 
    translateLabel l (a@(ACtrl c)) = ACtrl $ translateCtrl l c
    translateLabel l a = a
  
    translateCtrl l (c@(AGoto i)) = 
      case (Map.lookup i l) of
        Nothing -> c
        Just i' -> AGoto i'
    translateCtrl l c = c

labelsForFn (AAFDecl s) = Map.empty
labelsForFn (AAFDefn al s i) = getBadLabels Map.empty al

getBadLabels :: LabelMap -> [AAsm] -> LabelMap
getBadLabels l [] = l
getBadLabels l [x] = l
getBadLabels l ((ACtrl (ALabel i)):(ACtrl (AGoto i')):xs) = 
  getBadLabels (Map.insert i i' l) xs
getBadLabels l (x:xs) = getBadLabels l xs
