module Parser where

import Ast

import Prelude hiding (const)
import Data.Void
import Data.Functor.Identity
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

--------------------------------------------------------------------------------
--                                 Lexer                                      --
--------------------------------------------------------------------------------

-- Parser which simply produces some value without consuming any input tokens
produce :: a -> Parser a
produce val = val <$ string ""

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
--                               Operators                                    --
--------------------------------------------------------------------------------

assignop :: Parser AssignmentOperator
assignop =  AssignEq    <$ symbol "="   <|> AssignBitAnd <$ symbol "&="
        <|> AssignBitOr <$ symbol "|="  <|> AssignBitXor <$ symbol "^="
        <|> AssignOr    <$ symbol "||=" <|> AssignAnd    <$ symbol "&&="
        <|> AssignShl   <$ symbol ">>=" <|> AssignShr    <$ symbol "<<="

binopArithmetic :: Parser BinaryOperator
binopArithmetic =  OpAdd <$ symbol "+" <|> OpSub <$ symbol "-"
               <|> OpMul <$ symbol "*" <|> OpDiv <$ symbol "/"
               <|> OpMod <$ symbol "%"

binopBitwise :: Parser BinaryOperator
binopBitwise =  OpBitAnd <$ symbol "&" <|> OpBitOr <$ symbol "|"
            <|> OpBitXor <$ symbol "^" <|> OpShl   <$ symbol "<<"
            <|> OpShl   <$ symbol ">>"

binopLogical :: Parser BinaryOperator
binopLogical =  OpAnd <$ symbol "&&" <|> OpOr  <$ symbol "||"
            <|> OpEq <$ symbol "=="  <|> OpNeq <$ symbol "!="

binop :: Parser BinaryOperator
binop =  try (OpAssign <$> assignop)
     <|> binopArithmetic <|> binopBitwise <|> binopLogical



--------------------------------------------------------------------------------
--                              Identifiers                                   --
--------------------------------------------------------------------------------

_keywords :: [String]
_keywords = [
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
    check x = if   elem x _keywords
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

--------------------------------------------------------------------------------
--                             Declerations                                   --
--------------------------------------------------------------------------------

_qualifier :: Parser Qualifier
_qualifier =     Constexpr <$ keyword "constexpr"
            <|> Const      <$ keyword "const"
            <|> Static     <$ keyword "static"
            <|> Volatile   <$ keyword "volatile"
            <|> Mutable    <$ keyword "mutable"

_indirection :: Parser Indirection
_indirection =
      Reference <$ lexeme (char '&')
  <|> mkptr     <$> some (lexeme (char '*')) <*> checkLexeme (string "const")
  <|> Direct    <$  produce ""
  where
    mkptr :: String -> Bool -> Indirection
    mkptr ptr const = Pointer (length ptr) const

_varStorage :: Parser VariableStorage
_varStorage =  Bitfield <$ lexeme (char ':') <*> integer
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
declVar :: Parser [Decleration]
declVar = do
  -- Parse the "common" prefix, IE: qualifiers and typename
  qualifiers <- many _qualifier
  tname      <- typename
  -- Parse the set of fields, seperated by ','
  vardecls   <- sepBy (vardecl qualifiers tname) comma
  -- End the "line" with ;
  _          <- semicolon
  return vardecls
  where
    vardecl :: [Qualifier] -> Typename -> Parser Decleration
    vardecl q t = (VariableDeclaration q t) <$> _indirection <*> identifier <*> _varStorage

--declFuncPointer :: Parser [Decleration]
--declFuncPointer = do
--  qualifiers <- many qualifier
--  tname      <- typename
--  varname    <- withinParens (symbol "*" *> identifier)
--  params     <- parameterList

--------------------------------------------------------------------------------
--                                 Literals                                   --
--------------------------------------------------------------------------------

-- Parses a sequence of characters quoted by some other, where the sequence
-- may include the quote character if it is prefixed by \
_quotedSeq :: Char -> (Parser String -> Parser [String]) -> Parser String
_quotedSeq c repeat =
  (concat) <$> between (char c) (char c) (repeat (escaped <|> nonCloser))
  where
    escaped   = try( string ('\\' : c : []))
    nonCloser = (:) <$> satisfy (/=c) <*> produce []

literalString :: Parser Literal
literalString = LiteralString <$> _quotedSeq '"' many

literalChar :: Parser Literal
literalChar = LiteralChar <$> _quotedSeq '\'' some

literalInt :: Parser Literal
literalInt = LiteralInt <$> L.signed sc L.decimal


-- _literalFloating :: (a -> Literal) -> Char -> Parser Float
_literalFloating litType c =
      try ((litType . realToFrac  ) <$> L.signed sc pfloat    <* end)
  <|> try ((litType . fromIntegral) <$> L.signed sc L.decimal <* (char '.' <* notFollowedBy digitChar) <* end)
  where
    pfloat :: Parser Float
    pfloat =  L.float
          <|> (\x -> fromIntegral(x) * 0.1) <$ (char '.') <*> L.decimal
    end :: Parser ()
    end = (() <$ lexeme (char c)) <|> notFollowedBy letterChar

literalDouble :: Parser Literal
literalDouble = _literalFloating LiteralDouble 'd'
literalFloat  :: Parser Literal
literalFloat  = _literalFloating LiteralFloat 'f'

--literalArray :: Parser Literal
--literalArray = withinBrackets (many expression)

literal :: Parser Literal
literal  =  try literalString
        <|> try literalChar
        <|> try literalDouble
        <|> try literalFloat
        <|> try literalInt

expression :: Parser Expression
expression = try (ExprLiteral <$> literal)
