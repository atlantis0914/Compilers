module Compile.IR.GenIR where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as Split
import Compile.Backend.Registers
import qualified Text.Parsec.Pos as P

import Compile.IR.CoalesceLabel

import qualified Debug.Trace as Trace

sourcePos = P.initialPos "garbage"

type Alloc = (Map.Map String Int, Int, Int, [AAsm])
-- (Map from idents -> tempNum, curTempNum)

type FnMap = Map.Map String ([IdentType], IdentType, Bool, Bool, Maybe Integer) -- Map of global fn defines

genFIR :: FnList -> FnMap -> [FnAAsm]
genFIR (FnList gdecls _) fnMap =
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

genFnAAsm :: FnMap -> GDecl -> Maybe FnAAsm
genFnAAsm fnMap (GFDefn (FDefn name args _ _ ast _) _) =
    Just $ AAFDefn (genIR fnMap ast alloc') name (length args)
  where
    alloc = (Map.empty, 0, 0, [])
    alloc' = foldl addArg alloc args
--     alloc' = case (Map.lookup (name) fnMap) of
--                Nothing -> error ("fuck" ++ name)
--                Just (_,_,_,_,True) -> foldl addArgInline alloc args
--                _ -> foldl addArg alloc args

-- genFnAasm (GFDecl (FDecl {

genFnAAsm _ (GFDecl (FDecl name _ _ _ isLib _) _) =
  if isLib then Just $ AAFDecl name
           else Nothing

genFnAAsm _ _ = Nothing

genIR :: FnMap -> AST -> Alloc -> [AAsm]
genIR fnMap (AST (Block stmts) _) alloc =
  let
    (_,_,_,aasm) = foldl (genStmt fnMap) alloc stmts
  in
    aasm

genStmt :: FnMap -> Alloc -> Stmt -> Alloc
genStmt _ alloc SNop = alloc
genStmt fm alloc (Ctrl c) = genCtrl fm alloc c
genStmt fnMap (m,i,l,aasm) (Expr e) = let
  (_,i',l',aasm') = genExp fnMap (m,i+1,l,[]) e (ATemp i)
  in (m,i',l',aasm ++ aasm')

genStmt fm (m,i,l,aasm) (Decl s t _ scope) = let
  m' = Map.insert s i m -- assign ident s, temp number i
  in genStmt fm (m',i+1,l,aasm) scope

genStmt fm (m,i,l,aasm) (Asgn var op e _ _) = let
  ident = getIdent var
  temp = ATemp $ m Map.! ident
  (_,i',l',aasm') = genExp fm (m,i,l,[]) e temp
  in (m,i',l',aasm ++ aasm')

genStmt fm (m,i,l,aasm) (Block stmts) = let
  -- Keep scope alive, start new AAsm list, concat when finished.
  (m',i',l',aasm') = foldl (genStmt fm) (m,i,l,[]) stmts
  in (m',i',l',aasm ++ aasm')

getIdent :: LValue -> String
getIdent (LExpr (Ident s _) _) = s
getIdent (LExpr (ExpUnMem _ (Ident s _) _) _) = s

genCtrl :: FnMap -> Alloc -> Ctrl -> Alloc

genCtrl fm (m,i,l,aasm) (Assert e _) = let
  -- Can't generate SourcePos so we can't exactly use the If code
  (_,i',el,eAasm) = genExp fm (m,i+1,l,[]) e (ATemp i)
  abortLabel = el
  endLabel = el + 1
  abortAasm = [ACtrl $ ALabel abortLabel] ++ [AFnCall "_abort" (ATemp i) [] []]
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


genExp :: FnMap -> Alloc -> Expr -> ALoc -> Alloc
genExp _ (varMap,n,l,aasm) (ExpInt num _ _) dest =
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ fromIntegral num]])
genExp _ (varMap,n,l,aasm) (ExpBool b _) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ABool b]])
genExp _ (varMap,n,l,aasm) (Ident s _) dest =
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ALoc $ ATemp $ varMap Map.! s]])

genExp f map (ExpBinOp op e1 e2 _) dest = genBinOp f map (op,e1,e2) dest
genExp f map (ExpRelOp op e1 e2 _) dest = genBinOp f map (op,e1,e2) dest
genExp f map (ExpLogOp op e1 e2 _) dest = genBinOp f map (op,e1,e2) dest
genExp f map (ExpPolyEq op e1 e2 _) dest = genBinOp f map (op,e1,e2) dest

genExp f alloc@(varMap,n,l,aasm) (ExpTernary e1 e2 e3 p) dest = let
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

genExp f (varMap,n,l,aasm) (ExpUnOp op e _) dest = let
  -- AAsm for operand
  (_,n1,l',aasm') = genExp f (varMap, n + 1,l,aasm) e (ATemp n)
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n]]
  in (varMap, n1, l', aasm' ++ c)

genExp f alloc@(varMap,n,l,aasm) e@(ExpFnCall fnName exprs _) dest =
  case (Map.lookup (getName fnName) f) of
    Nothing -> error ("Name was : " ++ (getName fnName))
    Just (_,_,_,_,Nothing) -> genRealFn f alloc e dest
    Just (_,_,_,_,Just i) -> genInlineFn f alloc e dest i

genExp f (varMap,n,l,aasm) (ExpNull _) dest =
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ 0]])

genExp f alloc@(varMap,n,l,aasm) e@(ExpAlloc t _) dest =
  genRealFn f alloc (ExpFnCall "calloc" [ExpInt (getIdentSize t) sourcePos Dec,
                                         ExpInt 1 sourcePos Dec] sourcePos) dest

genExp f alloc@(varMap,n,l,aasm) e@(ExpUnMem _ _ _) dest =
  alloc

getName :: String -> String
getName (('_'):('_'):('c'):('0'):('_'):xs) = xs
getName (('_'):xs) = xs
getName s = s

genInlineFn f alloc@(varMap, n, l, aasm) (ExpFnCall fnName exprs _) dest ret =
  let
    allocs = scanl (genExpAcc f) alloc exprs
    lenMinusOne = (length allocs) - 1
    locs = map toLoc (take lenMinusOne allocs)
    last@(varMap',n',l',aasm') = allocs !! lenMinusOne
    (aasm'', _) = foldl moveArgs (aasm', 0) locs
    newAasm = AAsm {aAssign = [dest], aOp = Nop, aArgs = [AImm (fromIntegral ret)]}
--     newAasm = AFnCall fnName dest locs
  in
    (varMap', n', l', aasm'' ++ [newAasm])

genRealFn f alloc@(varMap, n, l, aasm) (ExpFnCall fnName exprs _) dest =
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

genExpAcc :: FnMap -> Alloc -> Expr -> Alloc
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
