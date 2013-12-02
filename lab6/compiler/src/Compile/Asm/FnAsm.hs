module Compile.Asm.FnAsm where 

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

import Compile.Asm.AsmTypes
import Compile.Asm.ExprAsm

import Control.Monad.State

-- TODO : 
-- write struct accessor methods 

genAsmFnDecl :: IRFuncDef -> String 
genAsmFnDecl fdef@(IRFuncDef name args argTypes retType body argSizes) = 
  let
    (_, (fStr, _, _)) = runState (processBody fdef) ("", 2, Map.empty)
  in
    "  function " ++ name ++ 
      " (" ++ (commaSep args) ++ ")" ++ " {" ++ "\n" ++ 
      (argumentCoercion args) ++ 
      fStr ++ "\n" ++ 
    "  }\n"

commaSep :: [String] -> String
commaSep args = 
  concatMap (\x -> x) $ 
  mapInd (\x -> 
          \i -> (if (i == (length args) - 1) 
                   then x
                   else x ++ ", ")) args

argumentCoercion :: [String] -> String
argumentCoercion args = 
  concatMap (\x -> "  " ++ x ++ " = " ++ x ++ " | 0;" ++ "\n") args

processBody :: IRFuncDef -> AsmState ()
processBody (IRFuncDef {funcBody = body}) = do
  processAST body
  return ()

processAST :: IRAST -> AsmState ()
processAST ast@(IRAST stmt) = do
  let declSet = extractIdentAST ast
  processDeclVars (Set.toList declSet) 
  processStmt stmt

processStmt :: IRStmt -> AsmState ()
processStmt (IRBlock stmts) = do 
  indent <- getIndentation
--   (s,i,m) <- get 
--   let s' = s ++ indent ++ "{" ++ "\n"
--   put(s', i, m)
  processStmts stmts 
--   (s'',i', m') <- get
--   let s''' = s'' ++ "\n" ++ indent ++ "}"
--   put(s''', i', m')

processStmt (IRNop) = do
  return ()

processStmt (IRDecl dName dTyp dScp) = do
--  let s' = s ++ "var " ++ dName ++ " = 0;" ++ "\n"
--  put (s',i,m)
--  let m' = Map.insert dName i m
--  let i' = i + 1
--  put (s, i', m')
  processStmt dScp

processStmt (IRAsgn lval o rval) = do
  indent <- getIndentation
  lStr <- processIRExpr lval
  rStr <- processIRExpr rval
  (s,i,m) <- get
  let s' = s ++ indent ++ lStr ++ " = " ++ rStr ++ ";" ++ "\n"
  put (s',i,m)
  -- TODO
  return ()  

processStmt (IRCtrl ctrl) = do
  processCtrl ctrl

processCtrl :: IRCtrl -> AsmState ()
processCtrl (Return (Just rval) _) = do
  indent <- getIndentation
  rStr <- processIRExpr rval
  (s,i,m) <- get
  let s' = s ++ indent ++ "return (" ++ rStr ++ " | 0);" ++ "\n"
  put(s',i,m)

processCtrl (Return Nothing _) = do
  indent <- getIndentation
  (s,i,m) <- get
  let s' = s ++ indent ++ "return;" ++ "\n"
  put(s',i,m)

processCtrl (If e s1 s2 _) = do
  indent <- getIndentation
  (s,i,m) <- get
  eStr <- processIRExpr e
  let s' = s ++ indent ++ "if (" ++ eStr ++ ")" ++ "{" ++ "\n"
  put (s',i+1,m)
  processStmt s1
  (s'',i',m') <- get
  let s''' = s'' ++ indent ++ "}" ++ "\n" ++ indent ++ "else {" ++ "\n"
  put (s''',i',m') 
  processStmt s2
  (s'''',i'',m'') <- get
  let ls = s'''' ++ indent ++ "}" ++ "\n"
  put (ls,i''-1,m'')

processCtrl c = do 
  return ()

processCtrl (While e s1 _) = do
  indent <- getIndentation
  (s,i,m) <- get
  eStr <- processIRExpr e
  let s' = s ++ indent ++ "while (" ++ eStr ++ ")" ++ "{" ++ "\n"
  put (s',i+1,m)
  processStmt s1 
  (s'', i', m') <- get   
  let ls = s'' ++ indent ++ "}" ++ "\n"
  put (ls,i'-1,m')

--
--processCtrl (Assert e _) = do
--
--

processStmts :: [IRStmt] -> AsmState ()
processStmts [] = do
  return ()
processStmts (x:xs) = do
  processStmt x
  processStmts xs
  return ()

processDeclVars :: [String] -> AsmState ()
processDeclVars [] = do
  return ()
processDeclVars (x:xs) = do
  (s,i,m) <- get
  let s' = s ++ "    var " ++ x ++ " = 0;\n"
  put (s',i,m)
  processDeclVars xs

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

extractIdentAST :: IRAST -> Set.Set String
extractIdentAST (IRAST stmt) = extractIdentStmt stmt (Set.empty)

extractIdentStmt :: IRStmt -> Set.Set String -> Set.Set String
extractIdentStmt (IRDecl name typ inner) s = 
  extractIdentStmt inner (Set.insert name s)

extractIdentStmt (IRBlock stmts) s = 
  extractIdentStmts stmts s

extractIdentStmt st s = s

extractIdentStmts :: [IRStmt] -> Set.Set String -> Set.Set String
extractIdentStmts [] s = s
extractIdentStmts (x:xs) s = 
  (extractIdentStmts xs $ extractIdentStmt x s)

getIndentation :: AsmState String
getIndentation = do
  (s,i,m) <- get
  let ret = concatMap (\x -> x) $ replicate (i*2) " "
  return ret
  
