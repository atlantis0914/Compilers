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

parseAST :: FilePath -> ErrorT String IO AST
parseAST file = do
  code <- liftIOE $ BS.readFile file
  case parse astParser file code of
    Left e  -> throwError (show e)
    Right a -> return a

-- ParsecT s u m a is a monad transformer with
--   stream type s
--   user state type u,
--   underlying monad m
--   return type a

-- Note that : type Parsec s u = ParsecT s u Identity
-- All "Identity" does is to derive a Monad from a monad transformer,
-- as parsec is by default a monad transformer.

-- C0Parser AST is actually ParsecT ByteString () Identity AST
type C0Parser = Parsec ByteString ()

astParser :: C0Parser AST
astParser = do
  whiteSpace -- parse white-space before the program start
  reserved "int"
  reserved "main"
  parens $ return () -- parses int main ()
  ast <- braces (do
   pos   <- getPosition
   stmts <- many stmt
   return $ AST (Block stmts) pos)
  eof
  return ast
  <?> "block"

decl :: C0Parser Stmt
decl = do
   (typedecl "int")
   <|>
   (typedecl "bool")
   <?> "decl"

typedecl :: String -> C0Parser Stmt
typedecl s = do 
  pos <- getPosition
  dest <- reserved s
  ident <- declidentifier
  (do semi
      return $ Decl ident (toIdentType s) pos Nothing)
   <|>
   (do pos' <- getPosition
       op <- asnOp
       e <- expr
       semi
       return $ Decl ident (toIdentType s) pos (Just (Asgn ident op e pos')))
  <?> "typedecl"

asgn :: C0Parser Stmt
asgn = do
  pos  <- getPosition
  dest <- identifier
  op   <- asnOp
  e    <- expr
  semi
  return $ Asgn dest op e pos

ctrl :: C0Parser Stmt
ctrl = 
  ret 
  <|>
  ctrlIf
  <|>
  ctrlWhile

ctrlIf :: C0Parser Stmt
ctrlIf = do
  pos <- getPosition
  reserved "if" 
  e <- ctrlCondition
  s1 <- stmt
  (do s2 <- ctrlElseOpt
      return $ Ctrl (If e s1 s2 pos))
   <|>
   (do return $ Ctrl (If e s1 (Block []) pos))

ctrlCondition :: C0Parser Expr
ctrlCondition = parens expr

ctrlElseOpt :: C0Parser Stmt
ctrlElseOpt = do
  pos <- getPosition
  reserved "else"
  s1 <- stmt
  return s1

ctrlWhile :: C0Parser Stmt
ctrlWhile = do
  pos <- getPosition
  reserved "while"
  e <- ctrlCondition
  s1 <- stmt
  return $ Ctrl (While e s1 pos)

-- ctrlFor :: C0Parser Stmt
-- ctrlFor = do 
--   pos <- getPosition
--   reserved "for"
-- 
-- forCond = parens (
--   (do smp <- simp
--       semi
--       
   
  
  

ret :: C0Parser Stmt
ret = do
  pos <- getPosition
  reserved "return"
  e <- expr
  semi
  return $ Ctrl (Return e pos)

simp :: C0Parser Stmt
simp = 
  decl
  <|>
  asgn
  <|>
  stExpr
  <?> "simp"

stExpr :: C0Parser Stmt
stExpr = do 
  e <- expr 
  semi
  return $ Expr e

stmt :: C0Parser Stmt
stmt =
  simp
  <|>
  ctrl
  <|>
  block
  <?> "statement"

block :: C0Parser Stmt
block = braces (do
   pos   <- getPosition
   stmts <- many stmt
   return $ Block stmts)

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
               "="   -> Nothing
               x     -> fail $ "Nonexistent assignment operator: " ++ x)
   <?> "assignment operator"

-- Builds an expression using ops/precedence defined in opTable
expr :: C0Parser Expr
expr = buildExpressionParser opTable term <?> "expr"

term :: C0Parser Expr
term = 
   -- A term is either
   parens expr -- an expression surrounded by parenthesis
   <|>
   (do p <- getPosition
       i <- identifier
       return $ Ident i p) -- an identifier
   <|>
   (do p <- getPosition
       t <- reserved "true"
       return $ ExpBool True p)
   <|>
   (do p <- getPosition
       t <- reserved "false"
       return $ ExpBool True p)
   <|>
   (do p <- getPosition
       n <- Text.Parsec.try hex
       return $ ExpInt n p Hex)
   <|>
   (do whiteSpace
       p <- getPosition
       n <- dec
       return $ ExpInt n p Dec) -- or an integer
   <|>
   (do char '-'
       p <- getPosition
       e <- expr
       return $ ExpUnOp Neg (e) p)
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
c0Def :: GenLanguageDef ByteString () Identity
c0Def = LanguageDef
   {commentStart    = string "/*",
    commentStartStr = "/*",
    commentEnd      = string "*/",
    commentEndStr   = "*/",
    commentLine     = string "//",
    nestedComments  = True,
    identStart      = letter <|> char '_',
    identLetter     = alphaNum <|> char '_',
    opStart         = oneOf "=+-*/%&^|<>!~",
    opLetter        = oneOf "=&|<>",
    reservedNames   = ["int", "char", "string", "void", "while", "for", "if",
                       "return", "break", "continue", "NULL", "alloc",
                       "alloc_array", "typedef", "struct", "else", "assert",
                       "true", "false", "bool"],
    reservedOpNames = ["+",  "*",  "-",  "/",  "%", "?", 
                       ":", "->", ".", "--", "==", "!", 
                       "~", "++"],
    caseSensitive   = True}

c0Tokens :: Tok.GenTokenParser ByteString () Identity
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

opTable :: [[Operator ByteString () Identity Expr]]
opTable = [[prefix "-"  (ExpUnOp  Neg),
            prefix "~"  (ExpUnOp  BitwiseNot),
            prefix "!"  (ExpUnOp  LogicalNot)],
           [prefix "--" (ExpUnOp  Decr)],
           [binary "--" (ExpBinOp  Decr) AssocLeft],
           [binary "*"   (ExpBinOp Mul)  AssocLeft,
            binary "/"   (ExpBinOp Div)  AssocLeft,
            binary "%"   (ExpBinOp Mod)  AssocLeft],
           [binary "+"   (ExpBinOp Add)  AssocLeft,
            binary "-"   (ExpBinOp Sub)  AssocLeft]]
{-
We used a few helper functions which are in the Parsec documentation of Text.Parsec.Expr, located at \url{http://hackage.haskell.org/packages/archive/parsec/3.1.0/doc/html/Text-Parsec-Expr.html} The functions ``binary'', ``prefix'', and ``postfix'' were taken from there and are not my work, however they are used because rewriting them would look much the same, and they do not provide any core functionality, just make my code easier to read. Type signatures and location annotations were added by me.
-}

binary :: String -> (a -> a -> SourcePos -> a) -> Assoc -> Operator ByteString () Identity a
binary  name f = Infix $ do pos <- getPosition
                            reservedOp name
                            return $ \x y -> f x y pos

prefix :: String -> (a -> SourcePos -> a) -> Operator ByteString () Identity a
prefix  name f = Prefix $ do pos <- getPosition
                             reservedOp name
                             return $ \x -> f x pos
