module Compile.IR.InlineFn where 

import Compile.Types 
import Compile.IR.GenIR(FnMap)

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Control.Monad.State

import qualified Debug.Trace as Trace

-- Bool is whether or not this should be inlined
type FnAllocs = (Map.Map String (Alloc, Bool))
type ILInnerState = (FnAllocs, Alloc, [AAsm])
type ILState = State ILInnerState

tuningThreshold :: Int
tuningThreshold = 50

-- Threshold for the largest fn to try and inline onto
maxInlineThresh :: Int
maxInlineThresh = 400

inlineFns :: FnMap -> [FnAAsm] -> [FnAAsm]
inlineFns fnMap fnaasm =
  let
    fnAllocs = foldl (genFnAllocs fnMap) Map.empty fnaasm
  in
    map (fnmagic fnAllocs) fnaasm

fnmagic :: FnAllocs -> FnAAsm -> FnAAsm
fnmagic fnAlc a@(AAFDecl s) = a
fnmagic fnAlc a@(AAFPreInline alloc name i) =
  if (lengthGood) 
    then AAFDefn finAAsm name i
    else AAFDefn oldAAsm name i
  where
    (_, finLoc, finAAsm) = execState inlineAlloc (fnAlc, alloc, [])
    lengthGood = (length oldAAsm < maxInlineThresh)
    (_,_,_,_,oldAAsm) = alloc

inlineAlloc :: ILState ()
inlineAlloc = do 
  (fnAlc, (sc, vs, n, l, aasms), rest) <- get
  case (aasms) of 
    [] -> return ()
    (x:xs) -> (do
      put (fnAlc, (sc, vs, n, l, xs), rest)
      inlineAAsm x
      return ()) 

inlineAAsm :: AAsm -> ILState ()
inlineAAsm asm@(AFnCall fName dest aList lives) = do
  (fnAlc, proc, fin) <- get
  case (Map.lookup fName fnAlc) 
    of (Just (_, False)) -> (do 
          put (fnAlc, proc, fin ++ [asm])
          inlineAlloc)
       (Just (alloc, True)) -> (do 
          generateInline alloc asm
          inlineAlloc)
       Nothing -> (do
          put (fnAlc, proc, fin ++ [asm])
          inlineAlloc)

inlineAAsm asm = do
  (fnAlc, proc, fin) <- get
  put (fnAlc, proc, fin ++ [asm])
  inlineAlloc

generateInline :: Alloc -> AAsm -> ILState ()
generateInline al@(sc, vMap, n, l, aasms) a@(AFnCall fName dest args lives) = do
  (fnAlc, proc, fin) <- get
  put (fnAlc, proc, fin ++ [a])
  let aasms' = drop (length args) aasms
  let aasms'' = init aasms'
  -- We've dropped the argument moves, and the ret.
  -- arguments are 0 -> (length args) - 1
  finishInlining al aasms'' args dest 
  Trace.trace (fName ++ "\n" ++ show aasms ++ "\n" ++ show args) $ (return ())

finishInlining :: Alloc -> [AAsm] -> [ALoc] -> ALoc -> ILState ()
finishInlining (sc,vm,n,l,am) aasms args dest = do
  (fnAlc, proc, fin) <- get
  let n' = n + (length args)
  let aasms' = map (incrSt n l dest) aasms
  let argMoves = genArgMoves n args
  let aasms'' = argMoves ++ aasms'
  put (fnAlc, proc, fin ++ aasms'')
  return ()

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

genArgMoves :: Int -> [ALoc] -> [AAsm]
genArgMoves n alist = 
  mapInd (\arg -> \i -> AAsm [ATemp (n + i) False] Nop [ALoc $ arg]) alist

incrSt :: Int -> Int -> ALoc -> AAsm -> AAsm
incrSt n l d (AAsm asgn o args) = 
  (AAsm (map (incrStALoc n l d) asgn) o (map (incrStAVal n l d) args))
incrSt n l d (ACtrl (ARet _)) = error ("shouldn't hit ctrl")
incrSt n l d (ACtrl (ALabel i)) = (ACtrl (ALabel (i + l)))
incrSt n l d (ACtrl (AIf v i s)) = ACtrl $ AIf (incrStAVal n l d v) i s
incrSt n l d (ACtrl (AGoto i)) = ACtrl $ AGoto (i + l)
incrSt n l d (AFnCall _ _ _ _) = error ("shouldn't hit fn call")

incrStALoc :: Int -> Int -> ALoc -> ALoc -> ALoc
incrStALoc n l d (AReg 0 b) = d
incrStALoc n l d (ATemp i b) = (ATemp (n + i) b)
incrStALoc n l d a = a

incrStAVal :: Int -> Int -> ALoc -> AVal -> AVal
incrStAVal n l d (ALoc l') = ALoc (incrStALoc n l d l')
incrStAVal n l d a = a
 
genFnAllocs :: FnMap -> FnAllocs -> FnAAsm -> FnAllocs
genFnAllocs fMap fnAlc (AAFPreInline alloc name i) = 
  fnAlc `seq` Map.insert name (alloc, shouldInlineAlloc alloc) fnAlc

shouldInlineAlloc :: Alloc -> Bool
shouldInlineAlloc (sc, varMap, n, l, aasms) = 
  let
    c1 = ((length aasms < tuningThreshold) && (length aasms > 0))
    c2 = (singleReturn aasms)
    c3 = (lastReturn aasms) -- examples of different inline preds
  in
    c1 && c2

singleReturn :: [AAsm] -> Bool
singleReturn asm = res
  where 
    fin = foldl (\i -> \x ->
                   case (x) of 
                     ACtrl (ARet _) -> i+1
                     _ -> i) 0 asm
    res = (fin <= 1)

lastReturn :: [AAsm] -> Bool
lastReturn asm = res
  where 
    res = case (last asm) of
            ACtrl (ARet _) -> True
            _ -> False
