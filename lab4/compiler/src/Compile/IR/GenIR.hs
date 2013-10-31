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
    coalIR = coalesceLabel initIR
  in
    coalIR

addArg :: Alloc -> String -> Alloc
addArg alloc@(varMap, n, l, aasms) arg =
  let
    aasm = if n < 6 then [AAsm {aAssign = [ATemp n], aOp = Nop, aArgs = [ALoc $ AReg $ argArr !! n]}]
                    else [AAsm {aAssign = [ATemp n], aOp = Nop, aArgs = [ALoc $ AArg $ n - 6]}]
    aasms' = aasms ++ aasm
    varMap' = Map.insert arg n varMap
  in
    (varMap', n+1, l, aasms')

addArgInline :: Alloc -> String -> Alloc
addArgInline alloc@(varMap, n, l, aasms) arg =
  let
    aasm = [AAsm {aAssign = [ATemp n], aOp = Nop, aArgs = [ALoc $ AArg $ n]}]
    aasms' = aasms ++ aasm
    varMap' = Map.insert arg n varMap
  in
    (varMap', n+1, l, aasms')

genFnAAsm :: FnMap -> IRDecl -> Maybe FnAAsm
genFnAAsm fnMap (IRFDefn (IRFuncDef name args _ _ ast)) =
    Just $ AAFDefn (genIR fnMap ast alloc') name (length args)
  where
    alloc = (Map.empty, 0, 0, [])
    alloc' = foldl addArg alloc args

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
  (_,i',l',aasm') = genExp fnMap (m,i+1,l,[]) e (ATemp i)
  in (m,i',l',aasm ++ aasm')

genStmt fm (m,i,l,aasm) (IRDecl s t scope) = let
  m' = Map.insert s i m -- assign ident s, temp number i
  in genStmt fm (m',i+1,l,aasm) scope

genStmt fm (m,i,l,aasm) (IRAsgn (IRIdent s) _ e) = let
  temp = ATemp $ m Map.! s
  (m',i',l',aasm') = genExp fm (m,i,l,aasm) e temp
  in (m',i',l',aasm')

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpDereference (IRIdent s) t) _ e) = let
  temp = APtr (ATemp $ m Map.! s) Nothing 0
  dest = ATemp $ i
  (_,i',l',aasm') = genExp fm (m,i+1,l,aasm) e dest
  c = [AAsm [temp] Nop [ALoc dest]]
  in (m,i',l',aasm' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpDereference b t) _ e) = let
  b' = ATemp i
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) b b'
  dest = ATemp i'
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') e dest
  c = [AAsm [APtr b' Nothing 0] Nop [ALoc dest]]
  in (m'',i'',l'',aasm'' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpFieldSelect (IRExpDereference base _) _ _ size) op e) = let
  dest = ATemp $ i
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) e dest
  dest' = ATemp $ i'
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') base dest'
  c = [AAsm [APtr dest' Nothing size] Nop [ALoc dest]]
  in (m'',i'',l'',aasm'' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpFieldSelect base _ _ size) op e) = let
  dest = ATemp $ i
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) e dest
  dest' = ATemp $ i'
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') base dest'
  c = [AAsm [APtr dest' Nothing size] Nop [ALoc dest]]
  in (m'',i'',l'',aasm'' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpArraySubscript (IRIdent s) index t size) op e) = let
  dest = ATemp i
  temp = APtr (ATemp $ m' Map.! s) (Just AIndex) size
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) index dest
  dest' = ATemp $ i'
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') e dest'
  c = [AAsm [AIndex] Nop [ALoc $ dest], AAsm [temp] Nop [ALoc $ dest']]
  in (m'',i'',l'', aasm'' ++ c)

genStmt fm (m,i,l,aasm) (IRAsgn (IRExpArraySubscript base index t size) op e) = let
  base' = ATemp i
  (m',i',l',aasm') = genExp fm (m,i+1,l,aasm) base base'
  index' = ATemp i'
  (m'',i'',l'',aasm'') = genExp fm (m',i'+1,l',aasm') index index'
  val = ATemp $ i''
  (m''',i''',l''',aasm''') = genExp fm (m'',i''+1,l'',aasm'') e val
  c = [AAsm [AIndex] Nop [ALoc $ index'], AAsm [APtr base' (Just AIndex) size] Nop [ALoc $ val]]
  in (m''',i''',l''', aasm''' ++ c)

genStmt fm (m,i,l,aasm) (IRBlock stmts) = let
  -- Keep scope alive, start new AAsm list, concat when finished.
  (m',i',l',aasm') = foldl (genStmt fm) (m,i,l,[]) stmts
  in (m',i',l',aasm ++ aasm')

genStmt _ _ stmtm = error ("GENSTMT " ++ show stmtm)

genCtrl :: FnMap -> Alloc -> IRCtrl -> Alloc

genCtrl fm (m,i,l,aasm) (Assert e _) = let
  -- Can't generate SourcePos so we can't exactly use the If code
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i)
  abortLabel = el
  endLabel = el + 1
  abortAasm = [ACtrl $ ALabel abortLabel] ++ [AFnCall "abort" (ATemp i) [] []]
  outputAasm =
    eAasm
    ++ [ACtrl $ AIf (ALoc $ ATemp i) endLabel,
        ACtrl $ AGoto abortLabel]
    ++ abortAasm
    ++ [ACtrl $ ALabel endLabel]
  in
    (m, i', el + 2, aasm ++ outputAasm)

genCtrl fm (m,i,l,aasm) (If e s1 s2 _) = let
  -- store aasm for e in Temp(i)
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i)
  (m',i'',l',s1Aasm) = genStmt fm (m,i',el,[]) s1
  (m'',i''', l'',s2Aasm) = genStmt fm (m',i'',l',[]) s2
  s1Label = l''
  s2Label = l''+1
  endLabel = l''+2
  s1Aasm' = (ACtrl $ ALabel s1Label):s1Aasm ++ [ACtrl $ AGoto endLabel]
  s2Aasm' = (ACtrl $ ALabel s2Label):s2Aasm ++ [ACtrl $ AGoto endLabel]
  outputAasm =
    eAasm ++ -- assembly for e
    [ACtrl $ AIf (ALoc $ ATemp i) s1Label,
     ACtrl $ AGoto s2Label] -- Assembly for conditional jmp to s1 or s2
    ++ s1Aasm' -- Assembly for s1, including goto endLabel.
    ++ s2Aasm' -- Assembly for s2, including goto endLabel.
    ++ [ACtrl $ ALabel endLabel] -- Assembly for endLabel.
  in
    (m'',i''',l''+3, aasm ++ outputAasm)

genCtrl fm (m,i,l,aasm) (While e s1 _) = let
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i)
  (m',i'',l',s1Aasm) = genStmt fm (m,i',el,[]) s1
  startLabel = l'
  loopLabel = l' + 1
  endLabel = l' + 2
  s1Aasm' = (ACtrl $ ALabel loopLabel):s1Aasm ++ [ACtrl $ AGoto startLabel]
  outputAasm =
    [ACtrl $ ALabel startLabel] ++
    eAasm ++
    [ACtrl $ AIf (ALoc $ ATemp i) loopLabel,
     ACtrl $ AGoto endLabel] ++
    s1Aasm' ++
    [ACtrl $ ALabel endLabel]
  in
    (m',i'',l' + 3, aasm ++ outputAasm)

-- GenExps the expression into AReg 0 and then returns on AReg0
genCtrl fm (m,i,l,aasm) (Return (Just expr) _) =
  let
    (_,i',l',aasm') = genExp fm (m,i,l,[]) expr (AReg 0)
  in
    (m,i',l',aasm ++ aasm' ++ [ACtrl $ ARet (ALoc (AReg 0))])

genCtrl fm (m,i,l,aasm) (Return Nothing _) =
  -- dummy value
  (m,i,l,aasm ++ [ACtrl $ ARet (AImm 1)])


genExp :: FnMap -> Alloc -> IRExpr -> ALoc -> Alloc
genExp _ (varMap,n,l,aasm) (IRExpInt num _) dest =
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ fromIntegral num]])
genExp _ (varMap,n,l,aasm) (IRExpBool b) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ABool b]])
genExp _ (varMap,n,l,aasm) (IRIdent s) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ALoc $ ATemp $ varMap Map.! s]])

genExp f map (IRExpBinOp op e1 e2) dest = genBinOp f map (op,e1,e2) dest
genExp f map (IRExpRelOp op e1 e2) dest = genBinOp f map (op,e1,e2) dest
genExp f map (IRExpLogOp op e1 e2) dest = genBinOp f map (op,e1,e2) dest
genExp f map (IRExpPolyEq op e1 e2) dest = genBinOp f map (op,e1,e2) dest

genExp f alloc@(varMap,n,l,aasm) (IRExpTernary e1 e2 e3) dest = let
  (_,n1,e1l,e1Aasm) = genExp f (varMap,n+1,l,[]) e1 (ATemp n)
  (_,n2,e2l,e2Aasm) = genExp f (varMap,n1,e1l,[]) e2 dest
  (_,n3,e3l,e3Aasm) = genExp f (varMap,n2,e2l,[]) e3 dest
  e2Label = e3l
  e3Label = e3l + 1
  endLabel = e3l + 2
  e2Aasm' = (ACtrl $ ALabel e2Label):e2Aasm ++ [ACtrl $ AGoto endLabel]
  e3Aasm' = (ACtrl $ ALabel e3Label):e3Aasm ++ [ACtrl $ AGoto endLabel]
  outputAasm =
    e1Aasm ++
    [ACtrl $ AIf (ALoc $ ATemp n) e2Label,
     ACtrl $ AGoto e3Label]
    ++ e2Aasm'
    ++ e3Aasm'
    ++ [ACtrl $ ALabel endLabel]
  in
    (varMap,n3,e3l+3,aasm ++ outputAasm)

genExp f (varMap,n,l,aasm) (IRExpUnOp op e) dest = let
  -- AAsm for operand
  (_,n1,l',aasm') = genExp f (varMap, n + 1,l,aasm) e (ATemp n)
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n]]
  in (varMap, n1, l', aasm' ++ c)

genExp f alloc@(varMap,n,l,aasm) e@(IRExpFnCall fnName exprs) dest =
  case (Map.lookup (getName fnName) f) of
    Nothing -> error ("Name was : " ++ (getName fnName))
    Just (_,_,_,_,Nothing) -> genRealFn f alloc e dest
    Just (_,_,_,_,Just i) -> genInlineFn f alloc e dest i

genExp f (varMap,n,l,aasm) (IRExpNull) dest =
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ 0]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpAlloc t s) dest =
  genRealFn f alloc (IRExpFnCall "calloc" [IRExpInt (fromIntegral s) Dec,
                                           IRExpInt 1 Dec]) dest

genExp f alloc@(varMap,n,l,aasm) e@(IRExpAllocArray t expr s) dest = let
  (varMap',n',l',aasm') = genRealFn f alloc (IRExpFnCall "calloc" [IRExpInt (fromIntegral s) Dec,
                                           expr]) dest
  in (varMap',n',l',aasm' ++ [AAsm [AIndex] Nop [ALoc $ APtr dest Nothing 0]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpDereference (IRIdent s) t) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ALoc $ APtr (ATemp $ varMap Map.! s) Nothing 0]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpDereference expr _) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) expr (ATemp n)
  in (varMap',n'+1,l',aasm' ++ [AAsm [dest] Nop [ALoc $ APtr (ATemp n) Nothing 0]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpFieldSelect (IRExpDereference expr _) field t size) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) expr (ATemp n)
  in (varMap',n',l',aasm' ++ [AAsm [dest] Nop [ALoc $ APtr (ATemp n) Nothing size]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpFieldSelect base field t size) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) base (ATemp n)
  in (varMap',n',l',aasm' ++ [AAsm [dest] Nop [ALoc $ APtr (ATemp n) Nothing size]])

genExp f alloc@(varMap,n,l,aasm) e@(IRExpArraySubscript expr1 expr2 t o) dest = let
  (varMap',n',l',aasm') = genExp f (varMap,n+1,l,aasm) expr1 (ATemp n)
  (varMap'',n'',l'',aasm'') = genExp f (varMap',n'+1,l',aasm') expr2 (ATemp n')
  in (varMap'',n'',l'',aasm'' ++ [AAsm [AIndex] Nop [ALoc $ ATemp n'],
                                  AAsm [dest] Nop [ALoc $ APtr (ATemp n) (Just AIndex) o]])

genExp f alloc e dest = error (show e ++ " EXHAUST genExp")

genInlineFn f alloc@(varMap, n, l, aasm) (IRExpFnCall fnName exprs) dest ret =
  let
    allocs = scanl (genExpAcc f) alloc exprs
    lenMinusOne = (length allocs) - 1
    locs = map toLoc (take lenMinusOne allocs)
    last@(varMap',n',l',aasm') = allocs !! lenMinusOne
    (aasm'', _) = foldl moveArgs (aasm', 0) locs
    newAasm = AAsm {aAssign = [dest], aOp = Nop, aArgs = [AImm (fromIntegral ret)]}
  in
    (varMap', n', l', aasm'' ++ [newAasm])

genRealFn f alloc@(varMap, n, l, aasm) (IRExpFnCall fnName exprs) dest =
  let
    allocs = scanl (genExpAcc f) alloc exprs
    lenMinusOne = (length allocs) - 1
    locs = map toLoc (take lenMinusOne allocs)
    last@(varMap',n',l',aasm') = allocs !! lenMinusOne
    (aasm'', _) = foldl moveArgs (aasm', 0) locs
    newAasm = AFnCall fnName dest locs []
  in
    (varMap', n', l', aasm'' ++ [newAasm])

moveArgs :: ([AAsm], Int) -> ALoc -> ([AAsm], Int)
moveArgs (aasms, n) arg =
  let
    aasm = if n < 6 then [AAsm {aAssign = [AReg $ argArr !! n], aOp = Nop, aArgs = [ALoc arg]}]
                    else []
  in
    (aasms ++ aasm, n + 1)

toLoc :: Alloc -> ALoc
toLoc (_, n, _, _) =
  ATemp n

genExpAcc :: FnMap -> Alloc -> IRExpr -> Alloc
genExpAcc f (varMap,n,l,aasm) exp =
  genExp f (varMap,n+1,l,aasm) exp (ATemp n)

genBinOp f (varMap,n,l,aasm) (op,e1,e2) dest = let
  -- AAsm for left and right operand
  -- TODO: Make this more SSL friendly
  (_,n1,l',aasm') = genExp f (varMap, n + 1,l, []) e1 (ATemp n)
  (_,n2,l'',aasm'') = genExp f (varMap, n1 + 1,l', []) e2 (ATemp $ n1)
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n, ALoc $ ATemp $ n1]]
  -- Questionable variable indexing here
  in (varMap, n2, l'', aasm ++ aasm' ++ aasm'' ++ c)
