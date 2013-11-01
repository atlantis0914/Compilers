module Compile.IR.GenIR where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as Split
import Compile.Backend.Registers
import qualified Text.Parsec.Pos as P

import Compile.Util.IRUtil

import Compile.IR.CoalesceLabel

import qualified Debug.Trace as Trace

sourcePos = P.initialPos "garbage"

type Alloc = (Map.Map String Int, Int, Int, [AAsm])
-- (Map from idents -> tempNum, curTempNum)

type FnMap = Map.Map String ([IdentType], IdentType, Bool, Bool, Maybe Integer) -- Map of global fn defines

genFIR :: IRFnList -> FnMap -> [FnAAsm]
genFIR (IRFnList gdecls) fnMap =
  let
    initIR = Maybe.mapMaybe (genFnAAsm fnMap) gdecls
--    coalIR = coalesceLabel initIR
  in
    initIR
--    coalIR

addArg :: [Int] -> Alloc -> String -> Alloc
addArg argSizes alloc@(varMap, n, l, aasms) arg =
  let
    b = (argSizes !! n) == 8
    aasm = if n < 6 then [AAsm [ATemp n b] Nop [ALoc $ AReg (argArr !! n) b]]
                    else [AAsm [ATemp n b] Nop [ALoc $ AArg (n - 6) b]]
    aasms' = aasms ++ aasm
    varMap' = Map.insert arg n varMap
  in
    (varMap', n+1, l, aasms')

addArgInline :: [Int] -> Alloc -> String -> Alloc
addArgInline argSizes alloc@(varMap, n, l, aasms) arg =
  let
    b = (argSizes !! n) == 8
    aasm = [AAsm [ATemp n b] Nop [ALoc $ AArg n b]]
    aasms' = aasms ++ aasm
    varMap' = Map.insert arg n varMap
  in
    (varMap', n+1, l, aasms')

genFnAAsm :: FnMap -> IRDecl -> Maybe FnAAsm
genFnAAsm fnMap (IRFDefn (IRFuncDef name args _ _ ast argSizes)) =
    Just $ AAFDefn (genIR fnMap ast alloc') name (length args)
  where
    alloc = (Map.empty, 0, 0, [])
    alloc' = foldl (addArg argSizes) alloc args

genFnAAsm _ _ = Nothing

genIR :: FnMap -> IRAST -> Alloc -> [AAsm]
genIR fnMap (IRAST (IRBlock stmts)) alloc =
  let
    (_,_,_,aasm) = foldl (genStmt fnMap) alloc stmts
  in
    aasm

genStmt :: FnMap -> Alloc -> IRStmt -> Alloc
genStmt fm alloc (IRCtrl c) = genCtrl fm alloc c
genStmt fnMap (m,i,l,aasm) (IRExpr e) = let
  (_,i',l',aasm') = genExp fnMap (m,i+1,l,[]) e (ATemp i (getSize e))
  in (m,i',l',aasm ++ aasm')

genStmt fm (m,i,l,aasm) (IRDecl s t scope) = let
  m' = Map.insert s i m -- assign ident s, temp number i
  in genStmt fm (m',i+1,l,aasm) scope

genStmt fm (m,i,l,aasm) (IRAsgn (IRIdent s size) _ e) = let
  temp = ATemp (m Map.! s) (getSize (IRIdent s size))
  (m',i',l',aasm') = genExp fm (m,i,l,aasm) e temp
  in (m',i',l',aasm')

genStmt fm (m,i,l,aasm) (IRAsgn expr@(IRExpDereference (IRIdent s size) t _) _ e) = let
  temp = APtr (ATemp (m Map.! s) (getSize (IRIdent s size))) Nothing 0 0 (getSize expr)
  dest = ATemp i (getSize e)
  (_,i',l',aasm') = genExp fm (m,i+1,l,aasm) e dest
  c = [AAsm [temp] Nop [ALoc dest]]
  in (m,i',l',aasm' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn expr@(IRExpDereference b t s) _ e) = let
  b' = ATemp i (getSize b)
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) b b'
  dest = ATemp i' (getSize e)
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') e dest
  c = [AAsm [APtr b' Nothing 0 0 (getSize expr)] Nop [ALoc dest]]
  in (m'',i'',l'',aasm'' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpFieldSelect base _ _ size _) op e) = let
  dest = ATemp i (getSize e)
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) e dest
  dest' = ATemp i' (getSize base)
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') base dest'
  (tNum, ind, off, additive) = getPtrFromLastOp aasm''
  c = case ind of Just _ -> [AAsm [APtr (ATemp tNum True) ind (off) (additive + size) (getSize e)] Nop [ALoc dest]]
                  Nothing -> [AAsm [APtr (ATemp tNum True) ind (off + size) 0 (getSize e)] Nop [ALoc dest]]
  in (m'',i'',l'',aasm'' ++ c)
--Trace.trace ("Last op is : " ++ show (last aasm'') ++ " and " ++
--      " produced " ++ show c) $ (m'',i'',l'',aasm'' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpArraySubscript (IRIdent s _) index t size) op e) = let
  dest = ATemp i (getSize index)
  temp = APtr (ATemp (m' Map.! s) True) (Just (AIndex (getSize index))) 1 8 (getSize e)
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) index dest
  aasm'' = aasm' ++ genArrayCheck (ATemp (m' Map.! s) True) dest
  dest' = ATemp i' (getSize e)
  (m'',i'',l'',aasm''') = genExp fm (m',i'+1,l',aasm'') e dest'
  b = [AAsm [dest] Mul [ALoc $ dest, AImm size]]
  c = [AAsm [AIndex (getSize index)] Nop [ALoc $ dest], AAsm [temp] Nop [ALoc $ dest']]
  in (m'',i'',l'', aasm''' ++ b ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpArraySubscript base index t size) op e) = let
  base' = ATemp i (getSize base)
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) base base'
  index' = ATemp i' (getSize index)
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') index index'
  aasm''' = aasm'' ++  genArrayCheck base' index'
  val = ATemp i'' (getSize e)
  (m''',i''',l''',aasm'''') = genExp fm (m'',i''+1,l'',aasm''') e val
  b = [AAsm [index'] Mul [ALoc $ index', AImm size]]
  c = [AAsm [AIndex (getSize index)] Nop [ALoc index'], AAsm [APtr base' (Just (AIndex (getSize index))) 1 8 (getSize e)] Nop [ALoc $ val]]
  in (m''',i''',l''', aasm'''' ++ b ++ c)

genStmt fm (m,i,l,aasm) (IRBlock stmts) = let
  -- Keep scope alive, start new AAsm list, concat when finished.
  (m',i',l',aasm') = foldl (genStmt fm) (m,i,l,[]) stmts
  in (m',i',l',aasm ++ aasm')

genStmt _ _ stmtm = error ("GENSTMT " ++ show stmtm)

getSize :: IRExpr -> Bool
getSize (IRExpInt _ _) = False
getSize (IRExpBool _) = False
getSize (IRIdent _ size) = size == 8
getSize (IRExpBinOp _ e _) = False
getSize (IRExpRelOp _ e _) = False
getSize (IRExpLogOp _ e _) = False
getSize (IRExpPolyEq _ e _) = False
getSize (IRExpUnOp _ e) = getSize e
getSize (IRExpTernary _ e _) = getSize e
getSize (IRExpFnCall _ _ size) = size == 8
getSize IRExpNull = True
getSize (IRExpAlloc _ _) = True
getSize (IRExpAllocArray _ _ _) = True
getSize (IRExpArraySubscript _ _ _ size) = size >= 8
getSize (IRExpFieldSelect _ _ _ _ size) = size == 8
getSize (IRExpDereference _ _ size) = size == 8

genCtrl :: FnMap -> Alloc -> IRCtrl -> Alloc

genCtrl fm (m,i,l,aasm) (Assert e _) = let
  -- Can't generate SourcePos so we can't exactly use the If code
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i (getSize e))
  abortLabel = el
  endLabel = el + 1
  abortAasm = [ACtrl $ ALabel abortLabel] ++ [AFnCall "abort" (ATemp i (getSize e)) [] []]
  outputAasm =
    eAasm
    ++ [ACtrl $ AIf (ALoc $ ATemp i (getSize e)) endLabel Nothing,
        ACtrl $ AGoto abortLabel]
    ++ abortAasm
    ++ [ACtrl $ ALabel endLabel]
  in
    (m, i', el + 2, aasm ++ outputAasm)

genCtrl fm (m,i,l,aasm) (If e s1 s2 _) = let
  -- store aasm for e in Temp(i)
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i (getSize e))
  (m',i'',l',s1Aasm) = genStmt fm (m,i',el,[]) s1
  (m'',i''', l'',s2Aasm) = genStmt fm (m',i'',l',[]) s2
  s1Label = l''
  s2Label = l''+1
  endLabel = l''+2
  s1Aasm' = (ACtrl $ ALabel s1Label):s1Aasm ++ [ACtrl $ AGoto endLabel]
  s2Aasm' = (ACtrl $ ALabel s2Label):s2Aasm ++ [ACtrl $ AGoto endLabel]
  outputAasm =
    eAasm ++ -- assembly for e
    [ACtrl $ AIf (ALoc $ ATemp i (getSize e)) s1Label Nothing,
     ACtrl $ AGoto s2Label] -- Assembly for conditional jmp to s1 or s2
    ++ s1Aasm' -- Assembly for s1, including goto endLabel.
    ++ s2Aasm' -- Assembly for s2, including goto endLabel.
    ++ [ACtrl $ ALabel endLabel] -- Assembly for endLabel.
  in
    (m'',i''',l''+3, aasm ++ outputAasm)

genCtrl fm (m,i,l,aasm) (While e s1 _) = let
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i False)
  (m',i'',l',s1Aasm) = genStmt fm (m,i',el,[]) s1
  startLabel = l'
  loopLabel = l' + 1
  endLabel = l' + 2
  s1Aasm' = (ACtrl $ ALabel loopLabel):s1Aasm ++ [ACtrl $ AGoto startLabel]
  outputAasm =
    [ACtrl $ ALabel startLabel] ++
    eAasm ++
    [ACtrl $ AIf (ALoc $ ATemp i False) loopLabel Nothing,
     ACtrl $ AGoto endLabel] ++
    s1Aasm' ++
    [ACtrl $ ALabel endLabel]
  in
    (m',i'',l' + 3, aasm ++ outputAasm)

-- GenExps the expression into AReg 0 and then returns on AReg0
genCtrl fm (m,i,l,aasm) (Return (Just expr) _) =
  let
    (_,i',l',aasm') = genExp fm (m,i,l,[]) expr (AReg 0 (getSize expr))
  in
    (m,i',l',aasm ++ aasm' ++ [ACtrl $ ARet (ALoc (AReg 0 (getSize expr)))])

genCtrl fm (m,i,l,aasm) (Return Nothing _) =
  -- dummy value
  (m,i,l,aasm ++ [ACtrl $ ARet (AImm 1)])

genArrayCheck :: ALoc -> ALoc -> [AAsm]
genArrayCheck base index =
  [AAsm [AIndex False] Nop [ALoc index],
   AAsm [ASpill False] Lt [ALoc $ AIndex False, AImm 0],
   ACtrl $ AIf (ALoc $ ASpill False) 0 (Just "mem_error"),
   AAsm [ASpill False] Gte [ALoc $ AIndex False, ALoc $ APtr base Nothing 0 0 False],
   ACtrl $ AIf (ALoc $ ASpill False) 0 (Just "mem_error")]

genArrayAllocCheck :: ALoc -> [AAsm]
genArrayAllocCheck size =
  [AAsm [ASpill False] Lt [ALoc $ size, AImm 0],
   ACtrl $ AIf (ALoc $ ASpill False) 0 (Just "mem_error")]

genExp :: FnMap -> Alloc -> IRExpr -> ALoc -> Alloc
genExp _ (varMap,n,l,aasm) (IRExpInt num _) dest =
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ fromIntegral num]])
genExp _ (varMap,n,l,aasm) (IRExpBool b) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ABool b]])
genExp _ (varMap,n,l,aasm) (IRIdent s size) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ALoc $ ATemp (varMap Map.! s) (getSize $ IRIdent s size)]])

genExp f map (IRExpBinOp op e1 e2) dest = genBinOp f map (op,e1,e2) dest
genExp f map (IRExpRelOp op e1 e2) dest = genBinOp f map (op,e1,e2) dest
genExp f map (IRExpLogOp op e1 e2) dest = genBinOp f map (op,e1,e2) dest
genExp f map (IRExpPolyEq op e1 e2) dest = genBinOp f map (op,e1,e2) dest

genExp f alloc@(varMap,n,l,aasm) (IRExpTernary e1 e2 e3) dest = let
  (_,n1,e1l,e1Aasm) = genExp f (varMap,n+1,l,[]) e1 (ATemp n False)
  (_,n2,e2l,e2Aasm) = genExp f (varMap,n1,e1l,[]) e2 dest
  (_,n3,e3l,e3Aasm) = genExp f (varMap,n2,e2l,[]) e3 dest
  e2Label = e3l
  e3Label = e3l + 1
  endLabel = e3l + 2
  e2Aasm' = (ACtrl $ ALabel e2Label):e2Aasm ++ [ACtrl $ AGoto endLabel]
  e3Aasm' = (ACtrl $ ALabel e3Label):e3Aasm ++ [ACtrl $ AGoto endLabel]
  outputAasm =
    e1Aasm ++
    [ACtrl $ AIf (ALoc $ ATemp n False) e2Label Nothing,
     ACtrl $ AGoto e3Label]
    ++ e2Aasm'
    ++ e3Aasm'
    ++ [ACtrl $ ALabel endLabel]
  in
    (varMap,n3,e3l+3,aasm ++ outputAasm)

genExp f (varMap,n,l,aasm) (IRExpUnOp op e) dest = let
  -- AAsm for operand
  (_,n1,l',aasm') = genExp f (varMap, n + 1,l,aasm) e (ATemp n (getSize e))
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n (getSize e)]]
  in (varMap, n1, l', aasm' ++ c)

genExp f alloc@(varMap,n,l,aasm) e@(IRExpFnCall fnName exprs _) dest =
  case (Map.lookup (getName fnName) f) of
    Nothing -> error ("Name was : " ++ (getName fnName))
    Just (_,_,_,_,Nothing) -> genRealFn f alloc e dest
    Just (_,_,_,_,Just i) -> genInlineFn f alloc e dest i

genExp f (varMap,n,l,aasm) (IRExpNull) dest =
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ 0]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpAlloc t s) dest =
  genRealFn f alloc (IRExpFnCall "calloc" [IRExpInt (fromIntegral s) Dec,
                                           IRExpInt 1 Dec] 8) dest

genExp f alloc@(varMap,n,l,aasm) e@(IRExpAllocArray t expr s) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) expr $ ATemp n False

  locs = [ATemp n' False, ATemp n False]
  aasm'' = aasm' ++ genArrayAllocCheck (ATemp n False) ++
                    [AAsm [ATemp (n' + 1) False] Add [ALoc $ ATemp n False, AImm $ fromIntegral (max (8 `div` (max s 4)) 1)],
                     AAsm [ATemp n' False] Nop [AImm $ fromIntegral s],
                     AAsm [AReg 12 False] Nop [ALoc $ ATemp n' False],
                     AAsm [AReg 13 False] Nop [ALoc $ ATemp (n' + 1) False],
                     AFnCall "calloc" dest locs []]
  in Trace.trace ("Dest is : " ++ show dest) $ (varMap',n'+2,l',aasm'' ++ [AAsm [APtr dest Nothing 0 0 False] Nop [ALoc $ ATemp n False]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpDereference (IRIdent s _) t _) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ALoc $ APtr (ATemp (varMap Map.! s) True) Nothing 0 0 (getSize e)]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpDereference expr _ _) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) expr (ATemp n True)
  in (varMap',n'+1,l',aasm' ++ [AAsm [dest] Nop [ALoc $ APtr (ATemp n True) Nothing 0 0 (getSize e)]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpFieldSelect base field t size _) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) base (ATemp n (getSize base))
  (tNum, ind, off, additive) = getPtrFromLastOp aasm'
  c = case ind of Just _ -> [AAsm [dest] Nop [ALoc $ APtr (ATemp tNum True) ind (off) (additive + size) (getSize e)]]
                  Nothing -> [AAsm [dest] Nop [ALoc $ APtr (ATemp tNum True) ind (off + size) 0 (getSize e)]]
  in (varMap',n'+1,l',aasm' ++ c)

genExp f alloc@(varMap,n,l,aasm) e@(IRExpArraySubscript base index t size) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) base (ATemp n True)
  (varMap'',n'',l'',aasm'') = genExp f (varMap',n'+1,l',aasm') index (ATemp n' False)
  in (varMap'',n'',l'',aasm'' ++  (genArrayCheck (ATemp n True) (ATemp n' False)) ++
                                  [AAsm [ATemp n' False] Mul [ALoc $ ATemp n' False, AImm size]] ++
                                  [AAsm [AIndex False] Nop [ALoc $ ATemp n' False]] ++
                                  [AAsm [dest] Nop [ALoc $ APtr (ATemp n True) (Just $ AIndex False) 1 8 (getLocSize dest)]])

genExp f alloc e dest = error (show e ++ " EXHAUST genExp")

genInlineFn f alloc@(varMap, n, l, aasm) (IRExpFnCall fnName exprs _) dest ret =
  let
    allocs = scanl (genExpAcc f) alloc exprs
    lenMinusOne = (length allocs) - 1
    locs = map toLoc (zip (take lenMinusOne allocs) exprs)
    last@(varMap',n',l',aasm') = allocs !! lenMinusOne
    (aasm'', _) = foldl moveArgs (aasm', 0) locs
    newAasm = AAsm {aAssign = [dest], aOp = Nop, aArgs = [AImm (fromIntegral ret)]}
  in
    (varMap', n', l', aasm'' ++ [newAasm])

genRealFn f alloc@(varMap, n, l, aasm) (IRExpFnCall fnName exprs _) dest =
  let
    allocs = scanl (genExpAcc f) alloc exprs
    lenMinusOne = (length allocs) - 1
    locs = map toLoc (zip (take lenMinusOne allocs) exprs)
    last@(varMap',n',l',aasm') = allocs !! lenMinusOne
    (aasm'', _) = foldl moveArgs (aasm', 0) locs
    newAasm = AFnCall fnName dest locs []
  in
    (varMap', n', l', aasm'' ++ [newAasm])

getLocSize :: ALoc -> Bool
getLocSize (AReg _ b) = b
getLocSize (ATemp _ b) = b
getLocSize (ASpill b) = b
getLocSize (AUtil b) = b
getLocSize (AIndex b) = b
getLocSize (AArg _ b) = b
getLocSize (APtr _ _ _ _ b) = b
getLocSize (AMem _ b) = b

moveArgs :: ([AAsm], Int) -> ALoc -> ([AAsm], Int)
moveArgs (aasms, n) arg =
  let
    aasm = if n < 6 then [AAsm [AReg (argArr !! n) (getLocSize arg)] Nop [ALoc arg]]
                    else []
  in
    (aasms ++ aasm, n + 1)

toLoc :: (Alloc, IRExpr) -> ALoc
toLoc ((_, n, _, _), e) =
  ATemp n (getSize e)

genExpAcc :: FnMap -> Alloc -> IRExpr -> Alloc
genExpAcc f (varMap,n,l,aasm) exp =
  genExp f (varMap,n+1,l,aasm) exp (ATemp n (getSize exp))

genBinOp f (varMap,n,l,aasm) (op,e1,e2) dest = let
  -- AAsm for left and right operand
  -- TODO: Make this more SSL friendly
  (_,n1,l',aasm') = genExp f (varMap, n + 1,l, []) e1 (ATemp n (getSize e1))
  (_,n2,l'',aasm'') = genExp f (varMap, n1 + 1,l', []) e2 (ATemp n1 (getSize e2))
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n (getSize e1), ALoc $ ATemp n1 (getSize e2)]]
  -- Questionable variable indexing here
  in (varMap, n2, l'', aasm ++ aasm' ++ aasm'' ++ c)

getPtrFromLastOp aasm =
  case (last aasm) of
    aasm@(AAsm _ _ [src]) -> extractPtrAasm aasm src
    p -> error ("Got something fucked up in getPtrFromLastOp")

extractPtrAasm _ (ALoc (APtr (ATemp i True) Nothing off _ _)) = (i, Nothing, off, 0)
extractPtrAasm _ (ALoc (APtr (ATemp i True) (Just (AIndex False)) off additiveOffset _)) = (i, Just (AIndex False), off, additiveOffset)
extractPtrAasm aasm s = error ("Got s instead of expected expr in extractPtrAasm : " ++ show aasm)

