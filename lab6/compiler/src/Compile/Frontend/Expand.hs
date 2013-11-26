module Compile.Frontend.Expand where 

import Compile.Types
import Compile.Frontend.ElaborateExpr

import qualified Debug.Trace as Trace

-- Performs an initial pass over the parse statements, converting and expanding 
-- assigns with an operation into a Nop assign with the ident as an lvalue. 
expandPStatements :: [ParseStmt] -> [ParseStmt]
expandPStatements stmts = concatMap expandPStatement stmts
  where expandPStatement s@(PAsgn lval Nothing e b p) = [PAsgn (expandLVal lval) Nothing (elabExpr e) b p]
        expandPStatement s@(PAsgn lval (Just op) e b p) = 
          if (funcCallInLVal lval) 
            then [s]
            else [PAsgn (expandLVal lval) (Nothing) (ExpBinOp op (lValToExpr (expandLVal lval)) (elabExpr e) p) b p]
        expandPStatement s@(PCtrl c) = [PCtrl $ expandCtrl c]
        expandPStatement s@(PBlock stmts) = let
          innerExpanded = expandPStatements stmts
          in 
            case innerExpanded of
              [stmt@(PBlock _)] -> [stmt]
              [stmt] -> [PBlock innerExpanded] -- OSMIUMMMM
              [] -> [] 
              x -> [PBlock innerExpanded]
        expandPStatement s@(PDecl id t p (Just asgn)) = [PDecl id t p Nothing] ++ (asgn'')
          where [asgn'@(PAsgn {})] = expandPStatement asgn
                asgn'' = [asgn' {pasgnShadow = True}]
        expandPStatement s@(PDecl id t p Nothing) = [s]
        expandPStatement s@(PExpr e) = [PExpr (elabExpr e)]

        collapseStmts stmts 
          | (length stmts == 1) = head stmts
          | otherwise = PBlock stmts

        expandCtrl (If e s1 s2 p) = If (elabExpr e) (collapseStmts $ expandPStatement s1) 
                                       (collapseStmts $ expandPStatement s2) p
        expandCtrl (While e s1 p) = While (elabExpr e) (collapseStmts $ expandPStatement s1) p
        expandCtrl (Return (Just e) p) = Return (Just (elabExpr e)) p
        expandCtrl (Assert e p) = Assert (elabExpr e) p
        expandCtrl (Return Nothing p) = Return Nothing p

        expandLVal l@(PLId _ _) = l
        expandLVal (PLMem mem p) = PLMem (expandLMem mem) p 
  
        expandLMem (Dot lv s p) = Dot (expandLVal lv) s p
        expandLMem (Arrow lv s p) = Arrow (expandLVal lv) s p
        expandLMem (Star lv p) = Star (expandLVal lv) p
        expandLMem (ArrayRef lv e p) = ArrayRef (expandLVal lv) (elabExpr e) p

funcCallInLVal :: PLValue -> Bool
funcCallInLVal (PLId s p) = False
funcCallInLVal (PLMem m p) = funcCallInMem m

funcCallInMem :: ParseMem -> Bool
funcCallInMem (Dot s _ _) = funcCallInLVal s
funcCallInMem (Arrow s _ _) = funcCallInLVal s
funcCallInMem (Star s _) = funcCallInLVal s
funcCallInMem (ArrayRef s e _) = funcCallInLVal s || funcCallInExpr e

funcCallInExpr :: Expr -> Bool
funcCallInExpr (ExpInt _ _ _) = False
funcCallInExpr (ExpBool _ _) = False
funcCallInExpr (ExpNull _) = False
funcCallInExpr (Ident _ _) = False
funcCallInExpr (ExpAlloc _ _) = False
funcCallInExpr (ExpAllocArray _ e1 _) = funcCallInExpr e1
funcCallInExpr (ExpBinOp o e1 e2 _) = funcCallInExpr e1 || funcCallInExpr e2
funcCallInExpr (ExpRelOp o e1 e2 _) = funcCallInExpr e1 || funcCallInExpr e2
funcCallInExpr (ExpLogOp o e1 e2 _) = funcCallInExpr e1 || funcCallInExpr e2
funcCallInExpr (ExpPolyEq o e1 e2 _) = funcCallInExpr e1 || funcCallInExpr e2
funcCallInExpr (ExpUnOp o e1 _) = funcCallInExpr e1 
funcCallInExpr (ExpTernary e1 e2 e3 _) = funcCallInExpr e1 || funcCallInExpr e2 ||
                                         funcCallInExpr e3
funcCallInExpr (ExpFnCall _ _ _) = True
funcCallInExpr (ExpBinMem o e1 e2 _) = funcCallInExpr e1 || funcCallInExpr e2
funcCallInExpr (ExpUnMem o e1 _) = funcCallInExpr e1 
funcCallInExpr e = error ("fuck : " ++ show e) 
