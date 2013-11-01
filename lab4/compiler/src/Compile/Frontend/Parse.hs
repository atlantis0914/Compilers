{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Maxime Serrano <mserrano@andrew.cmu.edu>

   The start of a parser.

   Note that this uses a modified version of Parsec. We do
   not advise replacing this modified version with a stock one.
-}
module Compile.Frontend.Parse where

import Control.Monad.Error
import Control.Monad.State
import Data.ByteString as BS
import Compile.Types

import Compile.Util.IdentTypeUtil

import Data.Int

import LiftIOE

import Text.ParserCombinators.Parsec
import Control.Monad.Identity                 -- For our custom Language Definition
import Text.Parsec hiding (parse)             -- Parser Combinators
import Text.Parsec.Expr                       -- Expression Parser Generator
import Text.Parsec.Token (GenLanguageDef(..)) -- Language Definition Structure
import qualified Text.Parsec.Token as Tok

import qualified Debug.Trace as Trace 

parseFnList :: FilePath -> ErrorT String IO ParseFnList
parseFnList file = do
  code <- liftIOE $ BS.readFile file
  case runP topLevelParser False file code of
    Left e  -> throwError (show e)
    Right a -> return a

type MState = Bool


type C0Parser = Parsec ByteString MState

-- Produces a list of ParseFns. These are either decls or definitions
topLevelParser :: C0Parser ParseFnList
topLevelParser = do 
  pos <- getPosition
  whiteSpace
  globalDecls <- many gdecl
  eof
  return $ ParseFnList globalDecls pos
  <?> "topLevel"

gdecl :: C0Parser PGDecl
gdecl = 
  typedefParser
  <|>
  (Text.Parsec.try structParser)
  <|>
  declDefnParser 

typedefParser :: C0Parser PGDecl
typedefParser = do
  pos <- getPosition
  reserved "typedef"
  t1 <- getType
  t2 <- getType
  semi
  return $ PTypeDef t1 t2 pos

structParser :: C0Parser PGDecl 
structParser = do
  pos <- getPosition
  reserved "struct"
  name <- identifier
  (do fields <- braces $ many getField
      semi
      return $ PSDefn (ParseSDefn name fields pos) pos)
   <|>
   (do semi
       return $ PSDecl (ParseSDecl name pos) pos)
  
declDefnParser :: C0Parser PGDecl
declDefnParser = do
  pos <- getPosition
  whiteSpace
  retType <- getType
  name <- identifier
  (paramTypes, params) <- getParamList
  -- Now we either have a declaration, or a definition. 
  (do semi
      -- We place False into the Decl as a place-holder. 
      return $ PFDecl (ParseFDecl name params paramTypes retType False pos) pos)
   <|>
   (do ast <- getAST
       return $ PFDefn (ParseFDefn {pfnName = name,
                                    pfnArgs = params,
                                    pfnArgTypes = paramTypes,
                                    pfnReturnType = retType,
                                    pfnBody = ast,
                                    pfnPos = pos}) pos)

getParamList = do 
  params <- parens $ commaSep getParam
  let (pt,pi) = Prelude.unzip params
  return (pt,pi) 

getParam = (do
  t <- getType
  i <- identifier
  return $ (t,i))

getField = (do
  (t,i) <- getParam
  semi
  return $ (t,i))

getType :: C0Parser IdentType 
getType = (do
  whiteSpace
  bTyp <- getBasicType
  bTyp' <- getComplexType bTyp
  return bTyp')

getComplexType :: IdentType -> C0Parser IdentType
getComplexType (t) = do
  whiteSpace
  (do char '*'
      t <- getComplexType (IPtr t)
      return t)
   <|>
   (do char '['
       whiteSpace
       char ']'
       t <- getComplexType (IArray t)
       return t)
   <|>
   (do return t)

getBasicType :: C0Parser IdentType
getBasicType =
  (do reserved "int"
      return $ IInt)
   <|>
   (do reserved "bool"
       return $ IBool)
   <|>
   (do reserved "void"
       return $ IVoid)
   <|>
   (do typ <- identifier
       return $ ITypeDef typ)
   <|>
   (do reserved "struct"
       typ <- identifier
       return $ IStruct (ITypeDef typ))
   
getAST :: C0Parser ParseAST
getAST = braces (do
  pos   <- getPosition
  stmts <- many stmt
  return $ ParseAST (PBlock stmts) pos)

astParser :: C0Parser ParseAST
astParser = do
  whiteSpace -- parse white-space before the program start
  reserved "int"
  reserved "main"
  parens $ return () -- parses int main ()
  ast <- braces (do
   pos   <- getPosition
   stmts <- many stmt
   return $ ParseAST (PBlock stmts) pos)
  eof
  return ast
  <?> "block"

decl :: C0Parser ParseStmt
decl = typedecl
  <?> "decl"

typedecl :: C0Parser ParseStmt
typedecl = do 
  pos <- getPosition
  idType <- getType
  ident <- declidentifier
  (do pos' <- getPosition
      op <- asnOp
      e <- expr
      return $ PDecl ident idType pos (Just (PAsgn (PLId ident pos) op e False pos')))
   <|>
   (do return $ PDecl ident idType pos Nothing)
  <?> "typedecl"


lvalue :: C0Parser PLValue
lvalue = 
  (do l <- (parens lvalue) 
      l' <- complexLValue l
      return l')
  <|>
  (do b <- basicLValue
      b' <- complexLValue b
      return b')

-- The left recursive bit 
basicLValue :: C0Parser PLValue
basicLValue = (do 
  pos <- getPosition
  name <- identifier
  return $ PLId name pos)
  <|>
  (do 
    char '*'
    whiteSpace
    pos <- getPosition
    l <- lvalue
    return $ PLMem (Star l pos) pos)

-- The non-left-recursive bits
complexLValue :: PLValue -> C0Parser PLValue
complexLValue lval = 
  do whiteSpace
     (do e <- brackets expr
         pos <- getPosition
         c <- complexLValue (PLMem (ArrayRef lval e pos) pos)
         return c)
     <|>
     (do (Text.Parsec.try (do char '-' -- Maybe I feel just a little bad. 
                              char '>'))
         whiteSpace
         i <- identifier
         pos <- getPosition
         c <- complexLValue (PLMem (Arrow lval i pos) pos)
         return c)
     <|>
     (do char '.'
         i <- identifier 
         pos <- getPosition
         c <- complexLValue (PLMem (Dot lval i pos) pos)
         return c)
     <|>
     (do return lval)
     <?> "complexLValue"

-- asgn :: C0Parser ParseStmt
-- asgn = do
--   pos  <- getPosition
--   dest <- lvalue
--   (do op   <- asnOp
--       e    <- expr
--       return $ PAsgn dest op e False pos)
--    <|>
--    (do op <- postOp
--        return $ PAsgn dest (Nothing) (expForPostOp dest op pos) False pos)
--    <?> "asgn"
--
asgn :: C0Parser ParseStmt
asgn = do
  pos  <- getPosition
  (dest, op) <- lValWithPostOp
  case (op) of 
    (Just o) -> (do return $ PAsgn dest (Nothing) (expForPostOp dest o pos) False pos)
    _ -> (do whiteSpace
             op   <- asnOp
             e    <- expr
             return $ PAsgn dest op e False pos)
   <?> "asgn"



expForPostOp :: PLValue -> Op -> SourcePos -> Expr 
expForPostOp i Incr p = ExpBinOp Add (lValToExpr i) (ExpInt 1 p Dec) p
expForPostOp i Decr p = ExpBinOp Sub (lValToExpr i) (ExpInt 1 p Dec) p

isLValueSimple :: PLValue -> Bool
isLValueSimple (PLId s _) = True
isLValueSimple (PLMem _ _) = False

lValWithPostOp :: C0Parser (PLValue, Maybe Op)
lValWithPostOp = 
  (do lv <- parens lvalue
      (do o <- postOp
          return (lv, Just o))
       <|>
       (do lv' <- complexLValue lv 
           return (lv', Nothing)))
   <|>
   (do lv <- lvalue
       case (lv) of
         (PLMem (Star _ _) _) -> return (lv, Nothing)
         _ -> (do o <- postOp
                  return (lv, Just o))
               <|>
               (do return (lv, Nothing)))


postOp :: C0Parser Op
postOp = do
  (do reserved "++"
      return $ Incr)
   <|>
   (do reserved "--"
       return $ Decr)

-- Parses a control flow structure
ctrl :: C0Parser ParseStmt
ctrl = 
  ret 
  <|>
  ctrlIf
  <|>
  ctrlWhile
  <|>
  ctrlFor
  <|>
  ctrlAssert

ctrlAssert :: C0Parser ParseStmt
ctrlAssert = do
  pos <- getPosition
  reserved "assert"
  e <- parens expr 
  semi
  return $ PCtrl (Assert e pos)

-- Parses a control flow 'if'
ctrlIf :: C0Parser ParseStmt
ctrlIf = do
  pos <- getPosition
  reserved "if" 
  e <- ctrlCondition
  s1 <- stmt
  (do s2 <- ctrlElseOpt
      return $ PCtrl (If e s1 s2 pos))
   <|>
   (do return $ PCtrl (If e s1 (PBlock []) pos))

-- Parses an expression surrounded by parens. 
-- Used as a ctrl flow condition
ctrlCondition :: C0Parser Expr
ctrlCondition = parens expr

-- Parses an optional else 
ctrlElseOpt :: C0Parser ParseStmt
ctrlElseOpt = do
  pos <- getPosition
  reserved "else"
  s1 <- stmt
  return s1

-- Parses a while loop
ctrlWhile :: C0Parser ParseStmt
ctrlWhile = do
  pos <- getPosition
  reserved "while"
  e <- ctrlCondition
  s1 <- stmt
  return $ PCtrl (While e s1 pos)

-- Parses an entire for loop and produces a while loop. 
ctrlFor :: C0Parser ParseStmt
ctrlFor = do 
  pos <- getPosition
  reserved "for"
  conds <- forCond
  forBody <- stmt
  return $ forToWhile conds forBody 

-- Parses the for-loop condition for (...) 
forCond :: C0Parser (Maybe ParseStmt, Expr, Maybe ParseStmt, SourcePos)
forCond = parens (do 
      pos <- getPosition
      c1 <- forSimpOpt
      semi
      e <- forExpr
      c2 <- forThirdParam
      return $ (c1,e,c2,pos))

-- Parses the first, third parameters out of a for-loop condition
forSimpOpt :: C0Parser (Maybe ParseStmt)
forSimpOpt = (do
  smp <- simp
  return $ Just smp)
  <|>
  (do return $ Nothing)

forThirdParam :: C0Parser (Maybe ParseStmt)
forThirdParam = 
  (Text.Parsec.try (do a <-asgn
                       return $ Just a))
  <|>
  (do s <- stExpr
      return $ Just s)
  <|>
  (do return $ Nothing)

-- Parses the expression (second parameter) out of a for-loop condition
forExpr :: C0Parser Expr
forExpr = do
  e <- expr
  semi
  return $ e

-- Elaborates a for into a while. 
forToWhile :: (Maybe ParseStmt, Expr, Maybe ParseStmt, SourcePos) -> ParseStmt -> ParseStmt
forToWhile (Nothing, e, Nothing, pos) s = PCtrl (While e s pos)
forToWhile (Just s1, e, Nothing, pos) s = PBlock [s1, PCtrl (While e s pos)]
forToWhile (Nothing, e, Just s2, pos) s = PCtrl (While e sApp pos)
  where sApp = PBlock [s, s2]
forToWhile (Just s1, e, Just s2, pos) s = PBlock [s1, PCtrl (While e sApp pos)]
  where sApp = PBlock [s, s2]

ret :: C0Parser ParseStmt
ret = do
  pos <- getPosition
  reserved "return"
  (do e <- expr
      semi
      return $ PCtrl (Return (Just e) pos))
   <|>
   (do semi
       return $ PCtrl (Return Nothing pos))

simp :: C0Parser ParseStmt
simp = 
  (Text.Parsec.try (do d <- decl
                       return d))
  <|>
  (Text.Parsec.try (do a <-asgn
                       return a))
  <|>
  (do s <- stExpr
      return s)
  <?> "simp"

-- There's a special case to consider here : namely the 
-- one brought up in the l4 handout of foo * bar. This is
-- either an expr with foo,bar as idents or the declaration
-- a pointer of type foo, named bar. We can easily special case
-- this and handle it in elaboration. 
stExpr :: C0Parser ParseStmt
stExpr = do 
  whiteSpace
  e <- expr
  return $ PExpr e

stmt :: C0Parser ParseStmt
stmt =
  block
  <|>
  (do s <- simp
      semi
      return s)
  <|>
  ctrl
  <?> "statement"

block :: C0Parser ParseStmt
block = braces (do
   pos   <- getPosition
   stmts <- many stmt
   return $ PBlock stmts)

-- Assignment Operators
asnOp :: C0Parser (Maybe Op)
asnOp = (do
   op <- operator
   return $ case op of
               "+="  -> Just Add
               "*="  -> Just Mul
               "-="  -> Just Sub
               "/="  -> Just Div
               "%="  -> Just Mod  
               "^="  -> Just BitwiseXOr
               "&="  -> Just BitwiseAnd
               "|="  -> Just BitwiseOr
               ">>=" -> Just RShift
               "<<=" -> Just LShift
               "="   -> Nothing
               x     -> fail $ "Nonexistent assignment operator: " ++ x)
   <?> "assignment operator"

-- Builds an expression using ops/precedence defined in opTable
expr' :: C0Parser Expr
expr' = buildExpressionParser opTable term <?> "expr"

expr :: C0Parser Expr
expr = do 
  pos <- getPosition
  e1 <- expr'
  (do (e2,e3) <- parseCond
      return $ ExpTernary e1 e2 e3 pos)
   <|>
   (do return e1)
--   s <- parseCond
--   case s of 
--     Nothing -> return $ e1
--     Just (e2,e3) -> return $ ExpTernary e1 e2 e3 pos

parseCond :: C0Parser (Expr, Expr)
parseCond = (do 
  reservedOp "?"
  e1 <- expr 
  whiteSpace
  reservedOp ":" 
  e2 <- expr
  return $ (e1,e2))

-- parseCond :: C0Parser (Maybe (Expr, Expr)) 
-- parseCond = (do 
--   reservedOp "?"
--   e1 <- expr 
--   whiteSpace
--   reservedOp ":" 
--   e2 <- expr
--   return $ Just (e1,e2))
--   <|>
--   (do return Nothing)

typExp :: C0Parser (IdentType, Expr)
typExp = do
  t <- getType
  comma
  e <- expr 
  return $ (t,e)

term :: C0Parser Expr
term = do
   whiteSpace
   -- A term is either
   (do e <- parens expr -- an expression surrounded by parenthesis
       return e)
   <|>
   (do p <- getPosition
       i <- identifier
       (do es <- parens $ commaSep expr
           return $ ExpFnCall i es p)
        <|>
        (do return $ Ident i p)) -- an identifier
   <|>
   (do p <- getPosition
       t <- reserved "true"
       return $ ExpBool True p)
   <|>
   (do p <- getPosition
       t <- reserved "false"
       return $ ExpBool False p)
   <|>
   (do p <- getPosition
       t <- reserved "NULL" 
       return $ ExpNull p)
   <|>
   (do p <- getPosition
       t <- reserved "alloc"
       typ <- parens getType
       return $ ExpAlloc typ p)  
   <|>
   (do p <- getPosition
       t <- reserved "alloc_array"
       (typ,exp) <- parens typExp
       return $ ExpAllocArray typ exp p)
   <|>
   (do p <- getPosition
       n <- Text.Parsec.try hex
       return $ ExpInt n p Hex)
   <|>
   (do whiteSpace
       p <- getPosition
       n <- dec
       return $ ExpInt n p Dec) -- or an integer
   <?> "term"

dec :: C0Parser Integer
dec = do n <- decimal
         whiteSpace
         return n

hex :: C0Parser Integer
hex = do char '0'
         n <- hexadecimal
         whiteSpace
         return n

-- Language Definition used by parser generating functions in Tok
c0Def :: GenLanguageDef ByteString MState Identity 
c0Def = LanguageDef
   {commentStart    = string "/*",
    commentStartStr = "/*",
    commentEnd      = string "*/",
    commentEndStr   = "*/",
    commentLine     = string "//",
    nestedComments  = True,
    identStart      = letter <|> char '_',
    identLetter     = alphaNum <|> char '_',
    opStart         = oneOf "+=-*/%&^|<>!~",
    opLetter        = oneOf "=&|<>",
    reservedNames   = ["int", "char", "string", "void", "while", "for", "if",
                       "return", "break", "continue", "NULL", "alloc",
                       "alloc_array", "typedef", "struct", "else", "assert",
                       "true", "false", "bool"],
    reservedOpNames = ["+",  "*",  "-",  "/",  "%", "?", 
                       ":", "->", ".", "--", "==", "!", 
                       "~", "++", ">", "<", ">>", "<<", 
                       "&&", "||", "!=", ">=", "<="],
    caseSensitive   = True}

c0Tokens :: Tok.GenTokenParser ByteString MState Identity
c0Tokens = Tok.makeTokenParser c0Def

-- Identifies that the string is reserved, and returns the updated monad
-- Note : A reserved word is treated as a single token using try.
reserved   :: String -> C0Parser ()
reserved   = Tok.reserved   c0Tokens

comma      :: C0Parser ()
comma      = do _ <- Tok.comma c0Tokens; return ()

-- A semicolon
semi       :: C0Parser ()
semi       = do _ <- Tok.semi c0Tokens; return ()

declidentifier :: C0Parser String
declidentifier = Tok.identifier c0Tokens

identifier :: C0Parser String
identifier = (parens identifier) <|> (Tok.identifier c0Tokens)

operator   :: C0Parser String
operator   = Tok.operator   c0Tokens

-- (braces p) parses p enclosed in braces '{', '}', and returns p
braces     :: C0Parser a -> C0Parser a
braces     = Tok.braces     c0Tokens

-- (parens p) parses p enclosed in parens '(', ')' and returns p
parens     :: C0Parser a -> C0Parser a
parens     = Tok.parens     c0Tokens

reservedOp :: String -> C0Parser ()
reservedOp = Tok.reservedOp c0Tokens

-- "The parsed number can be specified in decimal, hexadecimal or octal."
natural    :: C0Parser Integer
natural    = Tok.natural    c0Tokens

-- "The parsed number can be specified in decimal, hexadecimal or octal."
decimal :: C0Parser Integer
decimal = Tok.decimal c0Tokens

-- "The parsed number can be specified in decimal, hexadecimal or octal."
hexadecimal :: C0Parser Integer
hexadecimal = Tok.hexadecimal c0Tokens

-- Parses white space
whiteSpace :: C0Parser ()
whiteSpace = Tok.whiteSpace c0Tokens

commaSep   :: C0Parser a -> C0Parser [a]
commaSep   = Tok.commaSep   c0Tokens

semiSep    :: C0Parser a -> C0Parser [a]
semiSep    = Tok.semiSep    c0Tokens

brackets   :: C0Parser a -> C0Parser a
brackets   = Tok.brackets c0Tokens


--[binary "->" (ExpBinMem FDereference) AssocLeft,
--            binary "." (ExpBinMem Select) AssocLeft,
--            postfix (brackets expr) (ExpBinMem PArrayRef)],

arrayParser = do pos <- getPosition
                 e' <- brackets expr
                 return $ \x -> ExpBinMem PArrayRef x e' pos

dotParser = do pos <- getPosition
               char '.'
               i <- identifier
               return $ \x -> ExpBinMem Select x (Ident i pos) pos
  
arrowParser = do pos <- getPosition
                 whiteSpace
                 (do (Text.Parsec.try (do char '-'
                                          char '>')))
                 whiteSpace
                 i <- identifier
                 return $ \x -> ExpBinMem FDereference x (Ident i pos) pos

opTable :: [[Operator ByteString MState Identity Expr]]
opTable = [[postfix' $ choice [arrayParser,
                               dotParser,
                               arrowParser]],
           [prefix "-"  (ExpUnOp  Neg),
            prefix "~"  (ExpUnOp  BitwiseNot),
            prefix "!"  (ExpUnOp  LogicalNot),
            prefix "*"  (ExpUnMem PDereference)],
           [prefix "--" (ExpUnOp  Decr),  -- Throw errors
            prefix "++" (ExpUnOp  Incr)], -- in checkAST
           [binary "--"  (ExpBinOp Decr) AssocLeft],
           [binary "++"  (ExpBinOp Incr) AssocLeft],
           [binary "*"   (ExpBinOp Mul)  AssocLeft,
            binary "/"   (ExpBinOp Div)  AssocLeft,
            binary "%"   (ExpBinOp Mod)  AssocLeft],
           [binary "+"   (ExpBinOp Add)  AssocLeft,
            binary "-"   (ExpBinOp Sub)  AssocLeft], 
           [binary ">>"  (ExpBinOp RShift)  AssocLeft,
            binary "<<"  (ExpBinOp LShift)  AssocLeft],
           [binary ">"   (ExpRelOp Gt)   AssocLeft,
            binary ">="  (ExpRelOp Gte)  AssocLeft,
            binary "<"   (ExpRelOp Lt)   AssocLeft,
            binary "<="  (ExpRelOp Lte)  AssocLeft],
           [binary "=="  (ExpPolyEq Equ)  AssocLeft,
            binary "!="  (ExpPolyEq Neq)   AssocLeft],
           [binary "&"  (ExpBinOp BitwiseAnd)   AssocLeft],
           [binary "^"  (ExpBinOp BitwiseXOr)   AssocLeft],
           [binary "|"  (ExpBinOp BitwiseOr)   AssocLeft],
           [binary "&&"  (ExpLogOp And)   AssocLeft],
           [binary "||"  (ExpLogOp Or)   AssocLeft]]
{-
We used a few helper functions which are in the Parsec documentation of Text.Parsec.Expr, located at \url{http://hackage.haskell.org/packages/archive/parsec/3.1.0/doc/html/Text-Parsec-Expr.html} The functions ``binary'', ``prefix'', and ``postfix'' were taken from there and are not my work, however they are used because rewriting them would look much the same, and they do not provide any core functionality, just make my code easier to read. Type signatures and location annotations were added by me.
-}

binary :: String -> (a -> a -> SourcePos -> a) -> Assoc -> Operator ByteString MState Identity a
binary  name f = Infix $ do pos <- getPosition
                            reservedOp name
                            return $ \x y -> f x y pos

prefix :: String -> (a -> SourcePos -> a) -> Operator ByteString MState Identity a
prefix  name f = Prefix $ do pos <- getPosition
                             reservedOp name
                             return $ \x -> f x pos

postfix p f = Postfix $ do pos <- getPosition
                           e <- p
                           return $ \x -> f x e pos

postfix' p = Postfix . chainl1 p $ return (flip (.))
