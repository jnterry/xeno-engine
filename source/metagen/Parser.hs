module Parser where

import Ast

import Prelude hiding (const)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

--------------------------------------------------------------------------------
-- Basic Helpers

-- Parser which simply produces some value without consuming any input tokens
produce :: a -> Parser a
produce val = val <$ string ""

--------------------------------------------------------------------------------
-- Lexer

-- Space consumer, eats all whitespace, fails if there is no whitespace
-- to be eaten
sc :: Parser ()
sc = L.space (() <$ spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

-- A "lexeme" is some building block in the language, this parser should wrap
-- some other that knows how to parse the lexeme, then trailing whitespace will
-- be automatically consumed
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Tries to parse some lexeme, if possible the token is consumed and true is
-- returned. Else false is returned
checkLexeme :: Parser a -> Parser Bool
checkLexeme p = True <$ lexeme p <|> produce False

integer :: Parser Int
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol  = L.symbol sc

withinParens :: Parser a -> Parser a
withinParens = between (lexeme (char '(')) (lexeme (char ')'))

withinBrackets :: Parser a -> Parser a
withinBrackets = between (lexeme (char '[')) (lexeme (char ']'))

withinBraces :: Parser a -> Parser a
withinBraces = between (lexeme (char '{')) (lexeme (char '}'))

withinAngled :: Parser a -> Parser a
withinAngled = between (lexeme (char '<')) (lexeme (char '>'))

semicolon :: Parser ()
semicolon = () <$ lexeme (char ';')

comma :: Parser ()
comma = () <$ lexeme (char ',')

--------------------------------------------------------------------------------

-- List of keywords
keywords :: [String]
keywords = [
  "asm", "auto", "bool", "break", "case", "catch", "class", "const",
  "const_cast", "continue", "default", "delete", "do", "dynamic_cast",
  "else", "enum", "explicit", "extern", "for", "goto", "inline", "namespace",
  "new", "operator", "private", "public", "reinterpret_cast", "sizeof",
  "static_cast", "struct", "template", "throw", "try", "typeid", "union",
  "virtual", "volatile", "while"
  ]

keyword :: String -> Parser String
keyword w = (lexeme . try) (string w <* notFollowedBy (alphaNumChar <|> char '_'))

-- Parses an identifier, but does not consume trailing whitespace
identifierWord :: Parser String
identifierWord = try (p >>= check)
  where
    p = (:)
        <$> (letterChar <|> char '_')
        <*> many (letterChar <|> char '_' <|> digitChar)
    check x = if   elem x keywords
              then fail $ "Wanted identifier but got keyword " ++ show x
              else return x

-- Parses an identifer lexeme, eg, for a variable, type or function name
identifier :: Parser String
identifier  = lexeme identifierWord

-- Parses optionally fully qualified typename, for example, xen::Window
typename :: Parser Typename
typename = lexeme p
  where
    p = ((++) <$> segment <*> (p <|> produce ""))
    segment :: Parser String
    segment  = (++) <$> (string "::" <|> produce "") <*> identifierWord


qualifier :: Parser Qualifier
qualifier =     Constexpr <$ lexeme (keyword "constexpr")
            <|> Const     <$ lexeme (keyword "const")
            <|> Static    <$ lexeme (keyword "static")
            <|> Volatile  <$ lexeme (keyword "volatile")
            <|> Mutable   <$ lexeme (keyword "mutable")

indirection :: Parser Indirection
indirection =
      Reference <$ lexeme (char '&')
  <|> mkptr     <$> some (lexeme (char '*')) <*> checkLexeme (string "const")
  <|> Direct    <$  produce ""
  where
    mkptr :: String -> Bool -> Indirection
    mkptr ptr const = Pointer (length ptr) const

varStorage :: Parser VariableStorage
varStorage =  Bitfield <$ lexeme (char ':') <*> integer
          <|> try (FixedArray <$> withinBrackets integer)
          <|> try (FlexibleArray <$ symbol "[" <* symbol "]")
          <|> Standalone <$ produce ()

-- Parses a single "line" of struct field definitions
--
-- In the simple case this may just be "int x;"
--
-- "Indirection" is used to represent pointers and references, such as:
-- "int** const x;
--
-- This also supports multiple field definitions, eg:
-- int x, *ptr;
--
-- Note that the following is valid (but extreamly ugly) c++:
-- int& thing, * const test, array[5];
variableDeclaration :: Parser [VariableDeclaration]
variableDeclaration = do
  -- Parse the "common" prefix, IE: qualifiers and typename
  qualifiers <- many qualifier
  tname      <- typename
  -- Parse the set of fields, seperated by ','
  vardecls   <- sepBy (vardecl qualifiers tname) comma
  -- End the "line" with ;
  _          <- semicolon
  return vardecls
  where
    vardecl :: [Qualifier] -> Typename -> Parser VariableDeclaration
    vardecl q t = (VariableDeclaration q t) <$> indirection <*> identifier <*> varStorage
