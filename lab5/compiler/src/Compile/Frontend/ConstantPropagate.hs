module Compile.Frontend.ConstantPropagate where 

import Compile.Types 
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

import Control.Monad.State

import Compile.Util.IdentTypeUtil

type InnerState = (Set.Set String, Set.Set String, Map.Map String Integer)

type CFState = State InnerState

constantFold :: IRFnList -> IRFnList 
constantFold (IRFnList l) = IRFnList $ map processIRDecl l

processIRDecl :: IRDecl -> IRDecl
processIRDecl d@(IRSDefn s) = d
processIRDecl (IRFDefn fdef) = 
  let
    (fdef', state) = runState (processIRFDefn fdef) (Set.empty, Set.empty, Map.empty)
  in
    IRFDefn fdef'
  
processIRFDefn :: IRFuncDef -> CFState IRFuncDef 
processIRFDefn f@(IRFuncDef {funcBody = body}) = do
  body' <- processAST body
  return (f {funcBody = body'})

processAST :: IRAST -> CFState IRAST 
processAST (IRAST stmt) = do 
  stmt' <- processStmt stmt
  return $ IRAST stmt'

processStmt :: IRStmt -> CFState IRStmt
processStmt a@(IRNop) = do 
  return a
processStmt a@(IRAsgn (IRIdent asgn sz) Nothing (IRExpInt val b)) = do
  (curUsed, curMod, curConsts) <- get
  let curConsts' = Map.insert asgn val curConsts
  let curMod' = Set.insert asgn curMod
  let curUsed' = Set.insert asgn curUsed
  put (curUsed', curMod', curConsts')
  return a 

processStmt a@(IRAsgn lval op rexpr) = do
  (curUsed, curMod, curConsts) <- get
  -- replace constants in rexpr with values inside of map
  let lIdent = case (lval) of 
                 IRIdent asgn sz -> Just asgn
                 _ -> Nothing 
  let rexpr' = case (lIdent) of 
                 Just asgn -> cfExpr (Map.delete asgn curConsts) rexpr
                 _ -> cfExpr curConsts rexpr
  let (curUsed', curConsts') = case (lval) of 
                                 (IRIdent asgn sz) -> 
                                   (Set.insert asgn curUsed, Map.delete asgn curConsts)
                                 _ -> (curUsed, curConsts)
  put (curUsed', curMod, curConsts')
  return (IRAsgn lval op rexpr')

processStmt d@(IRDecl _ _ scp) = do 
  scp' <- processStmt scp
  return $ d {ideclScope = scp'}

processStmt (IRCtrl ctrl) = do
  ctrl' <- processCtrl ctrl
  return $ IRCtrl ctrl'

processStmt (IRBlock stmts) = do
  stmts' <- processStmts (stmts, [])
  return $ IRBlock stmts'

processStmt (IRExpr e) = do
  (curUsed, curMod, curConsts) <- get
  let e' = cfExpr curConsts e
  return $ IRExpr e'

processStmts :: ([IRStmt], [IRStmt]) -> CFState [IRStmt]
processStmts ([], rest) = do 
  return rest

processStmts (x:xs, rest) = do
  x' <- processStmt x
  rest' <- processStmts (xs, rest ++ [x'])
  return rest'

processCtrl :: IRCtrl -> CFState IRCtrl 
processCtrl (If e s1 s2 p) = do
  (curUsed, curMod, curConsts) <- get
  put (Set.empty, curMod, curConsts)
  -- replace consts in e
  s1' <- processStmt s1
  (s1Used, s1Mod, _) <- get
--  put (curUsed, curMod, curConsts)
  put (Set.empty, curMod, curConsts)
  s2' <- processStmt s2
  (s2Used, s2Mod, _) <- get
  let bothUsed = Set.union s1Used s2Used
  let curConsts' = removeKeys (Set.toList bothUsed) curConsts

  put (Set.union curUsed bothUsed, curMod, curConsts')  
  return $ If e s1' s2' p

processCtrl (While e s1 p) = do
  (curUsed, curMod, curConsts) <- get
  -- replace consts in e
--  put (curUsed, curMod, Map.empty)
  put (Set.empty, curMod, Map.empty)
  s1' <- processStmt s1
  (s1Used, s1Mod, _) <- get
  let curConsts' = removeKeys (Set.toList s1Used) curConsts
  put (Set.union curUsed s1Used , curMod, curConsts')
  return $ While e s1' p

processCtrl (Assert e p) = do
  -- replace consts in e
  return $ Assert e p

processCtrl r@(Return (Nothing) p) = do
  return $ r

processCtrl (Return (Just e) p) = do
  -- replace consts in e
  (curUsed, curMod, curConsts) <- get
  let e' = cfExpr curConsts e
  return $ Return (Just e') p

removeKeys :: [String] -> Map.Map String Integer -> Map.Map String Integer
removeKeys [] m = m
removeKeys (x:xs) m = removeKeys xs (Map.delete x m)

cfExpr m e@(IRIdent s sz) = 
  if (Map.member s m) 
    then IRExpInt (m Map.! s) Dec 
    else e
cfExpr m e@(IRExpBool b) = e
cfExpr m e@(IRExpInt _ _) = e
cfExpr m (IRExpBinOp o e1 e2) = IRExpBinOp o (cfExpr m e1) (cfExpr m e2)
cfExpr m (IRExpRelOp o e1 e2) = IRExpRelOp o (cfExpr m e1) (cfExpr m e2)
cfExpr m (IRExpLogOp o e1 e2) = IRExpLogOp o (cfExpr m e1) (cfExpr m e2)
cfExpr m (IRExpPolyEq o e1 e2) = IRExpPolyEq o (cfExpr m e1) (cfExpr m e2)
cfExpr m (IRExpUnOp o e1) = IRExpUnOp o (cfExpr m e1) 
cfExpr m (IRExpTernary e1 e2 e3) = IRExpTernary (cfExpr m e1) (cfExpr m e2) (cfExpr m e3)
cfExpr m e@(IRExpNull) = e
cfExpr m e@(IRExpAlloc _ _) = e
cfExpr m (IRExpAllocArray i e sz) = IRExpAllocArray i (cfExpr m e) sz
cfExpr m e@(IRExpArraySubscript e1 e2 i sz) = 
  IRExpArraySubscript (cfExpr m e1) (cfExpr m e2) i sz
cfExpr m e@(IRExpFieldSelect e1 s typ i1 i2) = 
  IRExpFieldSelect (cfExpr m e1) s typ i1 i2
cfExpr m (IRExpDereference e t s) = IRExpDereference (cfExpr m e) t s
cfExpr m (IRExpFnCall s elist i) = 
  IRExpFnCall s (map (cfExpr m) elist) i
--cfExpr m e = error ("rawr : " ++ show e)
