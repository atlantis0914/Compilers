{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Maxime Serrano <mserrano@andrew.cmu.edu>

   The start of a parser.

   Note that this uses a modified version of Parsec. We do 
   not advise replacing this modified version with a stock one.
-}
module Compile.Parse where

import Control.Monad.Error
import Data.ByteString as BS
import Compile.Types

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

type C0Parser = Parsec ByteString ()

astParser :: C0Parser AST
astParser = do
  whiteSpace
  reserved "int"
  reserved "main"
  parens $ return ()
  braces (do
   pos   <- getPosition
   decls <- many decl
   stmts <- many stmt
   return $ Block decls stmts pos)
   <?> "block"

decl :: C0Parser Decl
decl = do
   pos   <- getPosition
   reserved "int"
   ident <- identifier
   semi
   return $ Decl ident pos
   <?> "declaration"

stmt :: C0Parser Stmt
stmt = (do
   pos  <- getPosition
   dest <- identifier
   op   <- asnOp
   e    <- expr
   semi
   return $ Asgn dest op e pos)
   <|>
   (do pos <- getPosition
       reserved "return"
       e <- expr
       semi
       return $ Return e pos)
   <?> "statement"

asnOp :: C0Parser (Maybe Op)
asnOp = do
   op <- operator
   return $ case op of
               "+="  -> Just Add
               "*="  -> Just Mul
               "-="  -> Just Sub
               "/="  -> Just Div
               "%="  -> Just Mod
               "="   -> Nothing
               x     -> fail $ "Nonexistent assignment operator: " ++ x
   <?> "assignment operator"


expr :: C0Parser Expr
expr = buildExpressionParser opTable term <?> "expr"

term :: C0Parser Expr
term = do
   parens expr
   <|>
   (do p <- getPosition
       i <- identifier
       return $ Ident i p)
   <|>
   (do p <- getPosition
       n <- natural
       return $ ExpInt n p)
   <?> "term"

c0Def :: GenLanguageDef ByteString () Identity
c0Def = LanguageDef
   {commentStart    = string "/*",
    commentStartStr = "/*",
    commentEnd      = string "*/",
    commentEndStr   = "*/",
    commentLine     = string "#" <|> string "//",
    nestedComments  = True,
    identStart      = letter <|> char '_',
    identLetter     = alphaNum <|> char '_',
    opStart         = oneOf "=+-*/%&^|<>!~",
    opLetter        = oneOf "=&|<>",
    reservedNames   = ["int", "char", "string", "void", "while", "for", "if",
                       "return", "break", "continue", "NULL", "alloc",
                       "alloc_array", "typedef", "struct", "else", "assert",
                       "true", "false", "bool"],
    reservedOpNames = ["+",  "*",  "-",  "/",  "%", "?", ":", "->", "."],
    caseSensitive   = True}

c0Tokens :: Tok.GenTokenParser ByteString () Identity
c0Tokens = Tok.makeTokenParser c0Def

reserved   :: String -> C0Parser ()
reserved   = Tok.reserved   c0Tokens
comma      :: C0Parser ()
comma      = do _ <- Tok.comma c0Tokens; return ()
semi       :: C0Parser ()
semi       = do _ <- Tok.semi c0Tokens; return ()
identifier :: C0Parser String
identifier = Tok.identifier c0Tokens
operator   :: C0Parser String
operator   = Tok.operator   c0Tokens
braces     :: C0Parser a -> C0Parser a
braces     = Tok.braces     c0Tokens
parens     :: C0Parser a -> C0Parser a
parens     = Tok.parens     c0Tokens
reservedOp :: String -> C0Parser ()
reservedOp = Tok.reservedOp c0Tokens
natural    :: C0Parser Integer
natural    = Tok.natural    c0Tokens
whiteSpace :: C0Parser ()
whiteSpace = Tok.whiteSpace c0Tokens
commaSep   :: C0Parser a -> C0Parser [a]
commaSep   = Tok.commaSep c0Tokens
semiSep    :: C0Parser a -> C0Parser [a]
semiSep    = Tok.semiSep c0Tokens
brackets   :: C0Parser a -> C0Parser a
brackets   = Tok.brackets c0Tokens

opTable :: [[Operator ByteString () Identity Expr]]
opTable = [[prefix "-"   (ExpUnOp  Neg)],
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

