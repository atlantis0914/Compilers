module Compile.IR.GenIRAST where 

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Trace

import Compile.Util.IRUtil

import Compile.Util.IdentTypeUtil

type TypeMap = Map.Map String IdentType
-- Pulled from TypeCheck - should put this type shit in a utility file when cleaning up. 
type SMap = Map.Map String SDefn 
type FMap = Map.Map String ([IdentType], IdentType, Bool, Bool, Maybe Integer) 

toIRFnList :: FMap -> SMap -> FnList -> IRFnList
toIRFnList fMap sMap (FnList fns _) = IRFnList $ Maybe.mapMaybe (toIRDecl fMap sMap) fns

toIRDecl :: FMap -> SMap -> GDecl -> Maybe IRDecl
toIRDecl fMap sMap (GSDefn sdefn _) = Just (IRSDefn $ toIRStructDef fMap sMap sdefn)
toIRDecl fMap sMap (GFDefn fdefn _) = Just (IRFDefn $ toIRFuncDef fMap sMap fdefn)
toIRDecl _ _ _ = Nothing 

toIRStructDef :: FMap -> SMap -> SDefn -> IRStructDef
toIRStructDef _ _ (SDefn n f t o a s _) = IRStructDef n f t o a s

toIRFuncDef :: FMap -> SMap -> FDefn -> IRFuncDef 
toIRFuncDef fMap sMap (FDefn name args argTypes retType body _) = 
  IRFuncDef name args argTypes retType body' sizes
  where 
    tMap = Map.fromList $ zip args argTypes
    body' = toIRAST fMap sMap tMap body
    sizes = map getLongVsQuad argTypes 

toIRAST :: FMap -> SMap -> TypeMap -> AST -> IRAST
toIRAST fMap sMap tMap (AST stmt _) = IRAST stmt'
  where 
    stmt' = toIRStmt' fMap sMap tMap stmt

toIRStmt' :: FMap -> SMap -> TypeMap -> Stmt -> IRStmt 
toIRStmt' fm sm tm (Block stmts) = IRBlock (map (toIRStmt' fm sm tm) stmts)
toIRStmt' fm sm tm (Decl n typ _ scp) = 
  IRDecl n typ $ toIRStmt' fm sm (Map.insert n typ tm) scp
toIRStmt' fm sm tm (Ctrl c) = IRCtrl $ toIRCtrl fm sm tm c
toIRStmt' fm sm tm (Expr e) = IRExpr $ toIRExpr fm sm tm e
toIRStmt' fm sm tm (Asgn (LExpr e1 _) op e2 _ _) = IRAsgn (toIRExpr fm sm tm e1)
                                                     op (toIRExpr fm sm tm e2)
toIRStmt' _ _ _ SNop = IRNop
toIRStmt' _ _ _ e = error ("shit : " ++ show e)

toIRCtrl :: FMap -> SMap -> TypeMap -> Ctrl -> IRCtrl 
toIRCtrl fm sm tm (If e s1 s2 p) = If (toIRExpr fm sm tm e) 
                                   (toIRStmt' fm sm tm s1)
                                   (toIRStmt' fm sm tm s2) p
toIRCtrl fm sm tm (While e s1 p) = While (toIRExpr fm sm tm e) 
                                      (toIRStmt' fm sm tm s1) p
toIRCtrl fm sm tm (Assert e p) = Assert (toIRExpr fm sm tm e) p
toIRCtrl fm sm tm (Return Nothing p) = Return Nothing p
toIRCtrl fm sm tm (Return (Just e) p) = Return (Just $ toIRExpr fm sm tm e) p

toIRExpr :: FMap -> SMap -> TypeMap -> Expr -> IRExpr
toIRExpr fm sm tm e = e'
  where 
    (e', _) = toIRExpr' fm sm tm e

-- We produce the type of the expression we just checked. 
toIRExpr' :: FMap -> SMap -> TypeMap -> Expr -> (IRExpr, IdentType)
toIRExpr' fm sm tm (ExpInt i _ b) = (IRExpInt i b, IInt)
toIRExpr' fm sm tm (ExpBool b _) = (IRExpBool b, IBool)
toIRExpr' fm sm tm (Ident s _) = 
  case (Map.lookup s tm) of 
    Just t -> (IRIdent s (getLongVsQuad t), t)
    _ -> error ("error looking up ident : " ++ show s)
toIRExpr' fm sm tm (ExpNull _) = (IRExpNull, IPtr IAny)
-- We return a (typ *) after the alloc. 
toIRExpr' fm sm tm (ExpAlloc typ _) = (IRExpAlloc typ $ getSizeForTypeMap sm typ, IPtr typ)
--toIRExpr' fm sm tm (ExpAllocArray typ@(IStruct _) e _) = (IRExpAllocArray typ e' structArraySize, IPtr typ)
--  where 
--    structArraySize = 8
--    (e',_) = toIRExpr' fm sm tm e
toIRExpr' fm sm tm (ExpAllocArray typ e _) = (IRExpAllocArray typ e' size, IArray typ)
  where 
    size = (getSizeForTypeMap sm typ)
    (e',_) = toIRExpr' fm sm tm e
toIRExpr' fm sm tm (ExpBinOp o e1 e2 _) = (IRExpBinOp o e1' e2', t1)
  where 
    (e1',t1) = toIRExpr' fm sm tm e1
    (e2',_) = toIRExpr' fm sm tm e2
toIRExpr' fm sm tm (ExpRelOp o e1 e2 _) = (IRExpRelOp o e1' e2', IBool)
  where 
    (e1',_) = toIRExpr' fm sm tm e1
    (e2',_) = toIRExpr' fm sm tm e2
toIRExpr' fm sm tm (ExpPolyEq o e1 e2 _) = (IRExpPolyEq o e1' e2', IBool)
  where 
    (e1',_) = toIRExpr' fm sm tm e1
    (e2',_) = toIRExpr' fm sm tm e2
toIRExpr' fm sm tm (ExpUnOp o e _) = (IRExpUnOp o e', t)
  where 
    (e', t) = toIRExpr' fm sm tm e
toIRExpr' fm sm tm (ExpTernary e1 e2 e3 _) = (IRExpTernary e1' e2' e3', t)
  where 
    (e1', _) = toIRExpr' fm sm tm e1
    (e2', _) = toIRExpr' fm sm tm e2
    (e3', t) = toIRExpr' fm sm tm e3

toIRExpr' fm sm tm (ExpFnCall s elist _) = (IRExpFnCall s elist' datasize, typ)
  where 
    elist' = map (fst . toIRExpr' fm sm tm) elist
    (_,typ,_,_,_) = fm Map.! (getName s)
    datasize = getLongVsQuad typ

toIRExpr' fm sm tm (ExpBinMem Select e1 e2@(Ident field _) _) = 
  case (toIRExpr' fm sm tm e1) of
    (e1', IStruct (ITypeDef sName)) -> (IRExpFieldSelect e1' field typ offset datasize, typ)
    (e1', typ) -> error ("Didn't get struct return type in Select case of toIRExpr : " ++ show e1 ++ " and " ++ show e2 ++ "got " ++ show e1' ++ " and typ = " ++ show typ)
  where 
    (e1',e1Typ) = toIRExpr' fm sm tm e1
    typ = getTypeForStructField sm e1Typ field
    (size, offset) = getOffsetForStructField sm e1Typ field 
    datasize = getLongVsQuad typ

toIRExpr' fm sm tm (ExpBinMem PArrayRef e1 e2 _) = 
  case (toIRExpr' fm sm tm e1) of 
    (e1', IArray arrayTyp) -> (IRExpArraySubscript e1' e2' arrayTyp (getSizeForTypeMap sm arrayTyp), arrayTyp)
    _ -> error ("Didn't get array return type in PArrayRef case of toIRExpr'")
  where
    (e2', _) = toIRExpr' fm sm tm e2

toIRExpr' fm sm tm (ExpUnMem PDereference e1 _) = 
  case (toIRExpr' fm sm tm e1) of 
    (e1', IPtr innerTyp) -> (IRExpDereference e1' innerTyp (getLongVsQuad innerTyp), innerTyp)

toIRExpr' fm sm tm (ExpLogOp o e1 e2 _) = error ("Should no longer have log-ops in GenIRAST")

getOffsetForStructField :: SMap -> IdentType -> String -> (Int, Int)
getOffsetForStructField sm (IStruct (ITypeDef sName)) field = 
  case (Map.lookup sName sm) of 
    Just (SDefn {structOffsets = offsets}) -> offsets Map.! field
    _ -> error ("Couldn't find offset in getOffsetForStructField")

getTypeForStructField :: SMap -> IdentType -> String -> IdentType
getTypeForStructField sm (IStruct (ITypeDef sName)) field = 
  case (Map.lookup sName sm) of 
    Just (SDefn {structTypes = sTypes}) -> sTypes Map.! field
    _ -> error ("Couldn't find size in getTypeForStructField")

getSizeForTypeMap :: SMap -> IdentType -> Int
getSizeForTypeMap _ IInt = 4
getSizeForTypeMap _ IBool = 4
getSizeForTypeMap _ IVoid = error ("Trying to get size for void typeMap")
getSizeForTypeMap _ (IPtr _) = 8
getSizeForTypeMap _ (IArray _) = 8
getSizeForTypeMap sm (IStruct (ITypeDef name)) = 
  case (Map.lookup name sm) of
    Just (SDefn {structSize = size}) -> size
    _ -> error ("Struct " ++ name ++ " must be declared before use")
getSizeForTypeMap sm (ITypeDef name) = error ("Trying to get size for a type-def. Should be gone by GneIRAST : " ++ name)

getSizeForArrayRef :: IdentType -> Int
getSizeForArrayRef IInt = 4
getSizeForArrayRef IBool = 4
getSizeForArrayRef IVoid = error ("Trying to get size for void")
getSizeForArrayRef (IPtr _) = 8
getSizeForArrayRef (IArray _) = 8
getSizeForArrayRef (IStruct (ITypeDef _)) = 8
getSizeForArrayRef (ITypeDef name) = error ("Trying to get size for a type-def. Should be gone by GneIRAST : " ++ name)

getLongVsQuad :: IdentType -> Int
getLongVsQuad IInt = 4
getLongVsQuad IBool = 4
getLongVsQuad (IPtr _) = 8
getLongVsQuad (IArray _) = 8
getLongVsQuad IVoid = 4
getLongVsQuad (IStruct (ITypeDef name)) = 8
--error("Trying to get size for concrete struct : " ++ show name)
getLongVsQuad (ITypeDef name) = error ("Trying to get size for a type-def. Should be gone by GneIRAST : " ++ name)
