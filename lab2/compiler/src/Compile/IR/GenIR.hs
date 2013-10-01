module Compile.IR.GenIR where

import Compile.Types
import qualified Data.Map as Map

import qualified Debug.Trace as Trace

type Alloc = (Map.Map String Int, Int, Int, [AAsm])
-- (Map from idents -> tempNum, curTempNum)

genIR :: AST -> [AAsm]
genIR (AST (Block stmts) _) = 
  let 
    (_,_,_,aasm) = foldl genStmt (Map.empty, 0, 0, []) stmts
  in
    aasm

genStmt :: Alloc -> Stmt -> Alloc
genStmt alloc SNop = alloc
genStmt alloc (Ctrl c) = genCtrl alloc c 
genStmt (m,i,l,aasm) (Expr e) = let
  (_,_,l',aasm') = genExp (m,i+1,l,[]) e (ATemp i)
  in (m,i,l',aasm ++ aasm')

genStmt (m,i,l,aasm) (Decl s t _ scope) = let
  m' = Map.insert s i m -- assign ident s, temp number i
  in genStmt (m',i+1,l,aasm) scope

genStmt (m,i,l,aasm) (Asgn var op e _) = let
  temp = ATemp $ m Map.! var
  (_,_,l',aasm') = genExp (m,i,l,[]) e temp
  in (m,i,l',aasm ++ aasm')

genStmt (m,i,l,aasm) (Block stmts) = let
  -- Keep scope alive, start new AAsm list, concat when finished. 
  (m',i',l',aasm') = foldl genStmt (m,i,l,[]) stmts
  in (m',i',l',aasm ++ aasm')

genCtrl :: Alloc -> Ctrl -> Alloc 
genCtrl (m,i,l,aasm) (If e s1 s2 _) = let
  -- store aasm for e in Temp(i)
  (_,_,el,eAasm) = genExp (m,i+1,l,[]) e (ATemp i)
  (m',i',l',s1Aasm) = genStmt (m,i,el,[]) s1
  (m'',i'', l'',s2Aasm) = genStmt (m',i',l',[]) s2
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
    (m'',i'',l''+3, aasm ++ outputAasm)

genCtrl (m,i,l,aasm) (While e s1 _) = let
  (_,_,el,eAasm) = genExp (m,i+1,l,[]) e (ATemp i)
  (m',i',l',s1Aasm) = genStmt (m,i+1,l,[]) s1
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
    (m',i',l' + 3, aasm ++ outputAasm)

-- GenExps the expression into AReg 0 and then returns on AReg0
genCtrl (m,i,l,aasm) (Return expr _) = let
  (_,_,l',aasm') = genExp (m,i,l,[]) expr (AReg 0)
  in 
    (m,i,l',aasm ++ aasm' ++ [ACtrl $ ARet (ALoc (AReg 0))])

genExp :: Alloc -> Expr -> ALoc -> Alloc
genExp (varMap,n,l,aasm) (ExpInt num _ _) dest = 
  (varMap,n,l, aasm ++ [AAsm [dest] Nop [AImm $ fromIntegral num]])
genExp (varMap,n,l,aasm) (ExpBool b _) dest = 
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ABool b]])
genExp (varMap,n,l,aasm) (Ident s _) dest = 
  (varMap,n,l,aasm ++ [AAsm [dest] Nop [ALoc $ ATemp $ varMap Map.! s]])

genExp map (ExpBinOp op e1 e2 _) dest = genBinOp map (op,e1,e2) dest
genExp map (ExpRelOp op e1 e2 _) dest = genBinOp map (op,e1,e2) dest
genExp map (ExpLogOp op e1 e2 _) dest = genBinOp map (op,e1,e2) dest
genExp map (ExpPolyEq op e1 e2 _) dest = genBinOp map (op,e1,e2) dest

genExp alloc@(varMap,n,l,aasm) (ExpTernary e1 e2 e3 p) dest = let
  (_,_,e1l,e1Aasm) = genExp (varMap,n+1,l,[]) e1 (ATemp n)
  (_,_,e2l,e2Aasm) = genExp (varMap,n+2,e1l,[]) e2 dest
  (_,_,e3l,e3Aasm) = genExp (varMap,n+3,e2l,[]) e3 dest
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
    (varMap,n,e3l+3,aasm ++ outputAasm)
    
genExp (varMap,n,l,aasm) (ExpUnOp op e _) dest = let
  -- AAsm for operand
  (_,_,l',aasm') = genExp (varMap, n + 1,l,aasm) e (ATemp n)
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n]]
  in (varMap, n, l', aasm' ++ c)

genBinOp (varMap,n,l,aasm) (op,e1,e2) dest = let
  -- AAsm for left and right operand
  -- TODO: Make this more SSL friendly
  (_,_,l',aasm') = genExp (varMap, n + 1,l, []) e1 (ATemp n)
  (_,_,l'',aasm'') = genExp (varMap, n + 2,l', []) e2 (ATemp $ n + 1)
  -- AAsm for the operation
  c  = [AAsm [dest] op [ALoc $ ATemp n, ALoc $ ATemp $ n + 1]]
  -- Questionable variable indexing here
  in (varMap, n, l'', aasm ++ aasm' ++ aasm'' ++ c)
