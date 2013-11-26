module Compile.Frontend.TypeCheck where

import Compile.Types
import Compile.Util.IdentTypeUtil 

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import qualified Debug.Trace as Trace

--  The quad in FnMap represents the 
--  1. Argument Types
--  2. Return Type
--  3. Whether this is a library decl or not. (We | these together for multiple decls)
--  4. Whether this function has already been defined
--  5. Whether this is a good candidate for inlining. This is straight up spaghetti. 
type FnMap = Map.Map String ([IdentType], IdentType, Bool, Bool, Maybe Integer) -- Map of global fn defines 
type DeclMap = Map.Map String Bool  -- Map of what decls are currently in scope
type IdentMap = Map.Map String IdentType -- Map from idents -> types
type TDMap = Map.Map IdentType IdentType -- Typedef Map
type StructMap = Map.Map String SDefn -- Map from struct name -> struct definition. 

type Context = (IdentMap, FnMap, DeclMap, TDMap, StructMap, Bool)

-- Each function should be type checked independently 
checkTypeFnList :: FnList -> (Bool, [GDecl], FnMap, StructMap)
checkTypeFnList (FnList gdecls pos) = 
  let
    initFnMap = foldl genFnMap (Map.empty) gdecls
    gdecls' = foldl (squash initFnMap) [] gdecls
    (_, endMap, _, _, sMap, valid) = foldl checkGDecl (Map.empty, initFnMap, Map.singleton "main" True, baseIdentTypeMap, Map.empty, True) gdecls'
  in  
    case (Map.lookup "main" endMap) of
      Nothing -> error ("Error : int main() must be declared.")
      Just (argTypes, retType, lDecl, defn, _) -> (
        if ((length argTypes == 0) && (retType == IInt)) 
          then (valid, gdecls', (removeDecls endMap), sMap)
          else error ("Error : int main() must be the right type"))

removeDecls m = Map.filter (\(_,_,isLib,defn,_) -> (isLib || defn)) m

lTypesEqual :: [IdentType] -> [IdentType] -> Bool 
lTypesEqual l1 l2 = (all (\(t1,t2) -> typesEqual t1 t2) $ zip l1 l2) && 
                    (length l1 == length l2)


--validateFnArgs :: [IdentType] -> String -> Bool
validateFnArgs td sm typL fnName = 
  if (all (\t -> (not $ t == IVoid)) typL)
    then 
      if (all isSmallType typL)
        then 
          if (all (isValidConcreteType td sm) typL)
            then True
            else error ("Error : arguments not concrete in function " ++ fnName)
        else error ("Error : arguments not all small in function " ++ fnName)
    else error ("Error : cannot have void argument in function declaration of " ++ fnName)

squash :: FnMap -> [GDecl] -> GDecl -> [GDecl]
squash m prev (g@(GFDecl (FDecl {gdeclName = name, 
                                 gdeclIsLibrary = lib}) pos)) = 
  case (m Map.! name, lib) of
    (_,True) -> prev ++ [g]
    ((_,_,_,True, _), _) -> prev ++ [g]
    _ -> prev 
squash m prev g = prev ++ [g]

-- Used to create an initial function map. Also checks basic declaration
-- and redeclaration properties over the top-level program. 
genFnMap :: FnMap -> GDecl -> FnMap
-- Make these identities explicit so we'll explicitly fail when adding new 
-- top level constructs to our language. 
genFnMap fnMap (GTypeDef _ _ _) = fnMap
genFnMap fnMap (GSDecl _ _) = fnMap
genFnMap fnMap (GSDefn _ _) = fnMap
genFnMap fnMap (GFDecl (FDecl {gdeclName = name,
               gdeclArgTypes = argTypes, 
               gdeclReturnType = returnType, 
               gdeclIsLibrary = isLibrary}) pos) = 
  case (Map.lookup name fnMap) of 
    Just (oldArgs, oldRet, oldLib, isDeclared, _) -> 
      if ((lTypesEqual argTypes oldArgs) && (oldRet == returnType)) 
        then Map.insert name (oldArgs, oldRet, oldLib || isLibrary, isDeclared, Nothing) fnMap
        else error ("Error : function " ++ name ++ " redeclared incorrectly at " ++ (show pos))
    Nothing -> Map.insert name (argTypes, returnType, isLibrary, False, Nothing) fnMap

genFnMap fnMap (GFDefn (FDefn {fnName = name,
                               fnArgs = args,
                               fnArgTypes = argTypes,
                               fnReturnType = retType,
                               fnBody = body}) pos) = 
  case (Map.lookup name fnMap) of 
    Just (oldArgs, oldRet, oldLib, isDeclared, _) -> 
      if (isDeclared || oldLib) 
        -- One error message, two birds
        then error ("Error : function " ++ name ++ " redefined or library at " ++ show pos)
        else 
          if ((lTypesEqual argTypes oldArgs) && (oldRet == retType)) 
            then (Map.insert name (argTypes, retType, oldLib, True, shouldInline body) fnMap)
            else error ("Error : function " ++ name ++ " typed incorrectly at " ++ show pos)
    Nothing -> (Map.insert name (argTypes, retType, False, True, shouldInline body) fnMap)

checkGDecl :: Context -> GDecl -> Context
-- It's our responsibility here to ensure that the name being type-def'd
-- isn't already used as a function name. 
checkGDecl (ctx@(map, fnMap, dMap, tdMap, sMap, valid)) (GTypeDef t1 t2 pos) = 
  case t2 of 
    (ITypeDef s2) -> if (Map.lookup s2 fnMap == Nothing)
                       then (map, fnMap, dMap, Map.insert t2 t1 tdMap, sMap, valid)
                       else error ("Error : type name " ++ s2 ++ 
                                    " already used as fn name")
    _ -> error ("Typedef to concrete type at " ++ show pos)

checkGDecl (ctx@(map, fnMap, dMap, tdMap, sMap, valid)) 
           (GFDecl (FDecl {gdeclName = name,
                           gdeclArgTypes = argTypes, 
                           gdeclReturnType = returnType, 
                           gdeclIsLibrary = isLibrary}) pos) = 
  let
    argValid = validateFnArgs tdMap sMap argTypes name
    retValid = isSmallType returnType
  in
    (map, fnMap, Map.insert name True dMap, tdMap, sMap, valid && argValid && retValid)

-- Do nothing for struct decls. They are not useful in the least. 
checkGDecl ctx (GSDecl _ _) = ctx

checkGDecl ctx@(map, fnMap, dMap, tdMap, sMap, valid) 
          (GSDefn (sdefn@(SDefn {structName = name})) _) = 
  (map, fnMap, dMap, tdMap, Map.insert name sdefn sMap, valid)

-- We must set the map to EMPTY here - each function should start type
-- checking with just its arguments as the environment. 
checkGDecl (ctx@(_, fnMap, dMap, tdMap, sMap, valid)) 
      gdef@(GFDefn (FDefn {fnArgs = args,
                           fnName = name,
                           fnArgTypes = argTypes,
                           fnReturnType = retType, 
                           fnBody = body}) pos) = 
  let
    idMap = generateIdentContext args argTypes 
    argValid = validateFnArgs tdMap sMap argTypes name
    retValid = isSmallType retType
  in 
    checkASTTypes name (idMap, fnMap, Map.insert name True dMap, tdMap, sMap, valid && argValid && retValid) body 

generateIdentContext :: [String] -> [IdentType] -> Map.Map String IdentType
generateIdentContext args argTypes = 
  foldl (\m -> \(a,at) -> Map.insert a at m) (Map.empty) $ zip args argTypes


checkASTTypes :: String -> Context -> AST -> Context
checkASTTypes fName ctx (AST stmt _) = checkStmtValid fName ctx stmt

checkStmtValid :: String -> Context -> Stmt -> Context
checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Asgn lval@(LExpr e _) op expr b pos) =
  let
    tempCtx = if b 
                then (Map.delete (getIDLVal lval) map, fnMap, dMap, tdMap, sMap, valid) 
                else context
    maybeExprType = checkExprType fName tempCtx expr
--    maybeType = Map.lookup (getIDLVal name) map
    maybeType = checkExprType fName context e
    correctType = case (maybeType, maybeExprType) of
                    (Just t1, Just t2) -> 
                      let mType = coerceAny t1 t2 
                      in (Maybe.isJust $ mType) && (Maybe.maybe False isSmallType $ mType) 
                    (_, _) -> False
  in
    if correctType then (map, fnMap, dMap, tdMap, sMap, valid && correctType)
                   else error ("Error: Wrong type in assignment to " ++ (show lval) ++ " at " ++ show pos ++ " Expr: " ++ show expr ++ " got " ++ (show maybeType) ++ " and " ++ (show maybeExprType))

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Decl declName declType pos scope) =
  let
    validType = (not $ declType == IVoid) && (isValidConcreteType tdMap sMap declType)
    hasSmallType = isSmallType declType
    exists = Maybe.isNothing (Map.lookup declName map)
    map' = Map.insert declName declType map
    (_,_,_,_,_,checkAsgn) = checkStmtValid fName (map', fnMap, dMap, tdMap, sMap, valid) scope
  in
    if exists then (map', fnMap, dMap, tdMap, sMap, validType && valid && exists && checkAsgn && hasSmallType)
              else error ("Error: " ++ declName ++ " doesn't exist at " ++ show pos)

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Ctrl (Assert expr pos)) =
  let
    typeT = checkExprType fName context expr
    valid' = case checkExprType fName context expr of Nothing -> False
                                                      Just t -> t == IBool
  in
    if valid' then (map, fnMap, dMap, tdMap, sMap, valid && valid')
              else error ("Error : Assert expression is not a bool at " ++ show pos)

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Ctrl (If expr stmt1 stmt2 pos)) =
  let
    typeT = checkExprType fName context expr
    valid' = case checkExprType fName context expr of Nothing -> False
                                                      Just t -> t == IBool
    (_, _, _, _, _, valid'') = checkStmtValid fName context stmt1
    (_, _, _, _, _, valid''') = checkStmtValid fName context stmt2
  in
    if valid' then (map, fnMap, dMap, tdMap, sMap, valid' && valid'' && valid''' && valid)
              else error ("Error: If Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr ++ " type is : " ++ show typeT)

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Ctrl (While expr stmt pos)) =
  let
    (_, _, _, _, _, valid1) = checkStmtValid fName context stmt
    isBool = case checkExprType fName context expr of Nothing -> False
                                                      Just t -> t == IBool
  in
    if isBool then (map, fnMap, dMap, tdMap, sMap, valid1 && isBool && valid)
              else error ("Error: While Expression is not a bool at " ++ show pos ++ " AST:" ++ show expr ++ " got type : " ++ show (checkExprType fName context expr))

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Ctrl (Return mexpr pos)) =
  case (mexpr) of 
    Nothing -> checkReturnType fName context IVoid $ context
    Just expr -> (
      let
        isInt = case checkExprType fName context expr of Nothing -> False
                                                         Just t -> checkReturnType fName context t $ True
      in
        if isInt then (map, fnMap, dMap, tdMap, sMap, valid && isInt)
                 else error ("Error: Expression has incorrect return type at "))  -- ++ show pos ++ " AST" ++ show expr))

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Block stmts) =
  let
    (_, _, _, _, _, valid') = foldl (checkStmtValid fName) context stmts
  in
    (map, fnMap, dMap, tdMap, sMap, valid && valid')

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Expr fn@(ExpFnCall fnName subExps pos)) = 
  let
    checks = case checkFnCall context fn True of Nothing -> False 
                                                 Just t -> True
  in
    (map, fnMap, dMap, tdMap, sMap, valid && checks)

checkStmtValid fName (context@(map, fnMap, dMap, tdMap, sMap, valid)) (Expr expr) = 
  let 
    checks = case checkExprType fName context expr of Nothing -> False
                                                      Just t -> True
  in
    (map, fnMap, dMap, tdMap, sMap, valid && checks)

checkStmtValid _ context SNop = context

checkReturnType :: String -> Context -> IdentType -> a -> a
checkReturnType fnName (context@(map, fnMap, dMap, tdMap, sMap, valid)) t = 
  case (coerceAny retType t) of 
    (Just _) -> (\x -> x)
    Nothing -> error ("Error : Bad type for function with name : " ++ fnName ++ " expected " ++ show retType ++ " got " ++ show t)
  where 
    (_, retType, _, _, _) = fnMap Map.! fnName 

-- Watch out - we don't actually return result here? Not sure why it's like that
-- but it could be quite the problem yo.
matchType :: String -> Context -> Expr -> Expr -> [IdentType] -> IdentType -> (Maybe IdentType)
matchType f context expr1 expr2 expect result =
  let
    type1 = checkExprType f context expr1
    type2 = checkExprType f context expr2
  in
    case (type1, type2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just t1, Just t2) -> 
        case (coerceAnyExpect t1 t2 expect) of
          (Just _) -> Just result
          _ -> Nothing 

matchTypeEnsureSmall :: String -> Context -> Expr -> Expr -> IdentType -> (Maybe IdentType)
matchTypeEnsureSmall f context expr1 expr2 result =
  let
    type1 = checkExprType f context expr1
    type2 = checkExprType f context expr2
  in
    case (type1, type2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just t1, Just t2) -> 
        case (maybeIsSmallType $ coerceAny t1 t2) of
          (Just _) -> Just result
          _ -> Nothing 

-- if t1 == t2 && t1 `elem` expect then Just result
--                                                            else Nothing

typeEq :: String -> Context -> Expr -> Expr -> [IdentType] -> (Maybe IdentType)
typeEq fName context expr1 expr2 expect =
  let
    type1 = checkExprType fName context expr1
    type2 = checkExprType fName context expr2
  in
    case (type1, type2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just t1, Just t2) -> coerceAny t1 t2 


maybeIsSmallType :: Maybe IdentType -> Maybe IdentType
maybeIsSmallType (Just t1) = if (isSmallType t1) then (Just t1)
                                                 else Nothing 
maybeIsSmallType Nothing = Nothing 

--if t1 == t2 && t1 `elem` expect then Just t1
--                                                            else Nothing

typesEqual :: IdentType -> IdentType -> Bool
typesEqual t1 t2 = Maybe.isJust (coerceAny t1 t2)

coerceAny :: IdentType -> IdentType -> Maybe IdentType
coerceAny t1 t2 = 
  case (t1,t2) of 
    (IPtr IAny, IPtr t) -> Just t2
    (IPtr t, IPtr IAny) -> Just t1
    (_ , _) -> if (t1 == t2) then Just t1
                             else Nothing


coerceAnyExpect :: IdentType -> IdentType -> [IdentType] -> Maybe IdentType
coerceAnyExpect t1 t2 expect = 
  case (t1,t2) of 
    (IPtr IAny, IPtr t) -> 
      if (t2 `elem` expect) then Just t2
                            else Nothing
    (IPtr t, IPtr IAny) ->  
      if (t1 `elem` expect) then Just t1
                            else Nothing
    (_ , _) -> if (t1 == t2) && t1 `elem` expect then Just t1
                                                 else Nothing


checkExprIsType :: Maybe IdentType -> IdentType -> Maybe IdentType
checkExprIsType maybeType t =
  case maybeType of Nothing -> Nothing
                    Just t' -> if t' == t then Just t
                                          else Nothing

checkExprType :: String -> Context -> Expr -> Maybe IdentType 
checkExprType f ctx@(map, fnMap, dMap, tdMap, sMap, valid) e = 
  case (checkExprType' f ctx e) of
    (Just t) -> Just $ simplifyTypeDefdType tdMap t
    _ -> Nothing 

checkExprType' :: String -> Context -> Expr -> Maybe IdentType
checkExprType' _ _ (ExpInt _ _ _) = Just IInt
checkExprType' _ _ (ExpBool _ _) = Just IBool
checkExprType' _ (_,_,_,tdMap,sMap,_) (ExpAlloc t p) = 
  if (concreteTypeExists tdMap sMap t) 
    then Just $ IPtr t
    else error ("Didn't get a valid concrete type at : " ++ show p)
checkExprType' f ctx (ExpAllocArray t e _) = 
  case (checkExprType' f ctx e) of 
    Just IInt -> Just (IArray t)
    _ -> Nothing 
checkExprType' _ _ (ExpNull _) = Just $ IPtr IAny -- :t null = any*
checkExprType' _ (context@(map, _, _, _, _, _)) (Ident name _) = Map.lookup name map
checkExprType' f context (ExpBinOp _ expr1 expr2 _) =
  matchType f context expr1 expr2 [IInt] IInt
checkExprType' f context (ExpRelOp _ expr1 expr2 _) =
  matchType f context expr1 expr2 [IInt] IBool
checkExprType' f context (ExpLogOp _ expr1 expr2 _) =
  matchType f context expr1 expr2 [IBool] IBool
checkExprType' f context (ExpPolyEq _ expr1 expr2 _) =
  matchTypeEnsureSmall f context expr1 expr2 IBool
checkExprType' f context (ExpUnOp Neg expr _) =
  checkExprIsType (checkExprType f context expr) IInt
checkExprType' f context (ExpUnOp BitwiseNot expr _) =
  checkExprIsType (checkExprType f context expr) IInt
checkExprType' f context (ExpUnOp LogicalNot expr _) =
  checkExprIsType (checkExprType f context expr) IBool
checkExprType' f context (ExpTernary expr1 expr2 expr3 _) =
  case checkExprType f context expr1 of
    Nothing -> Nothing
    Just t -> if (t == IBool && isSmall)
                then resType
                else Nothing
  where 
    resType = typeEq f context expr2 expr3 [IInt, IBool]
    isSmall = case (resType) of 
                (Just typ) -> isSmallType typ
                Nothing -> False 

checkExprType' _ ctx@(map, fnMap, dMap, tdMap, sMap, valid) call@(ExpFnCall fnName subExps pos) = 
  checkFnCall ctx call False

checkExprType' f context (ExpUnMem PDereference e1 p) = 
  case checkExprType f context e1 of
    Just (IPtr IAny) -> error ("Error : cannot dereference indefinate type at " ++ show p)
    Just (IPtr t) -> Just t
    _ -> Nothing 

checkExprType' f ctx@(_,_,_,_,sMap,_) (ExpBinMem Select e1 e2 p) = 
  case (checkExprType f ctx e1, e2) of 
    (Just (IStruct (ITypeDef name)), Ident field _) -> 
      if (not $ Map.member name sMap) 
        then error ("Struct " ++ name ++ " mus be defined before use at " ++ show p)
        else typeForField (sMap Map.! name) field
    (t1,t2) -> error ("Have (t1,t2) = " ++ show t1 ++ " and " ++ show t2)

checkExprType' f ctx (ExpBinMem PArrayRef e1 e2 p) = 
  case (checkExprType f ctx e1, checkExprType f ctx e2) of 
    (Just (IArray t) , Just IInt) -> Just t
    (s,p) -> Nothing

checkExprType' f ctx e = error ("have " ++ show e) 

typeForField :: SDefn -> String -> (Maybe IdentType)
typeForField (SDefn {structTypes = sTyps}) field = Map.lookup field sTyps

checkFnCall ctx@(map, fnMap, dMap, tdMap, sMap, valid) (ExpFnCall fnName subExps pos) canBeVoid = 
  case (Map.lookup fnName dMap, Map.lookup fnName fnMap) of 
    (Nothing, _) -> error ("Error : Function : " ++ fnName ++ " used undeclared at " ++ show pos)
    (_, Just (argTypes, retType, libDecl, isDecl, _)) -> 
      if (isDecl || libDecl) -- Then we're good, just make sure non-void ret
        then if (validateFnCall ctx fnName argTypes subExps retType canBeVoid pos) 
               then Just retType
               else error ("Error : " ++ fnName ++ " invocation has problems at " ++ show pos)
        else error ("Error : " ++ fnName ++ " has not been declared yet at " ++ show pos)

consumeType :: Maybe IdentType -> IdentType
consumeType Nothing = error "No type"
consumeType (Just t) = t

-- validateFnCall :: Context -> String -> [IdentType] -> [Expr] -> IdentType -> Bool
validateFnCall (ctx@(idMap, _, _, tdMap, _, _)) fnName argTypes argExprs retType canBeVoid pos = 
  let
    recTypes = map (consumeType . (checkExprType fnName ctx)) argExprs
    match = all (\(t1,t2) -> typesEqual t1 t2) $ zip argTypes recTypes
    isShadowed = (Map.lookup fnName idMap == Nothing) 
  in
    if (not match)
      then error ("Error : Function types don't match on invocation of : " ++ fnName ++ " at " ++ show pos)
      else (
    if ((retType == IVoid) && (not canBeVoid))
      then error ("Error : Cannot use invoke void function : " ++ fnName ++ " at " ++ show pos)
      else (
    if (not $ length argTypes == length recTypes)
      then error ("Error : problem with parameters during invocation of : " ++ fnName ++ " at " ++ show pos)
      else (
    if (not isShadowed) 
      then error ("Error : function name, " ++ fnName ++ ", shadowed at " ++ show pos)
      else True)))


shouldInline :: AST -> Maybe Integer
shouldInline (AST stmt pos) = isGoodToInline stmt 

isGoodToInline :: Stmt -> Maybe Integer
isGoodToInline (Ctrl (Return (Just (ExpInt i _ _)) _)) = Just i
isGoodToInline (Decl _ _ _ sc) = isGoodToInline sc
isGoodToInline (Block [Decl _ _ _ sc]) = isGoodToInline sc
isGoodToInline (Block [s]) = isGoodToInline s
isGoodToInline _ = Nothing
