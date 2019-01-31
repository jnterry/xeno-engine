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

integer :: Parser Integer
integer = lexeme L.decimal

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

structField :: Parser StructField
structField = StructField
              <$> many qualifier
              <*> typename
              <*> indirection
              <*> identifier
