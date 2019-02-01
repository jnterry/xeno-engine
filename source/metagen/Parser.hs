module Parser where

import Ast

import Prelude hiding (const)
import Data.Void
import Data.List
import Data.Functor.Identity
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

--------------------------------------------------------------------------------
--                             Combinators                                    --
--------------------------------------------------------------------------------

-- Parser which simply produces some value without consuming any input tokens
produce :: a -> Parser a
produce val = val <$ string ""

-- Tries to do some parse, if successful returns Just the value, if fails
-- returns Nothing
optionalMaybe :: Parser a -> Parser (Maybe a)
optionalMaybe p = try(Just <$> p) <|> produce Nothing

--------------------------------------------------------------------------------
--                                 Lexer                                      --
--------------------------------------------------------------------------------

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
--                            Binay Operators                                 --
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

binopCmp :: Parser BinaryOperator
binopCmp =  OpLe <$ symbol "<="  <|> OpGe  <$ symbol ">="
        <|> OpLt <$ symbol "<"   <|> OpGt  <$ symbol ">"


binop :: Parser BinaryOperator
binop =  try binopLogical
     <|> try (OpAssign <$> assignop)
     <|> try binopBitwise
     <|> try binopCmp
     <|> try binopArithmetic

--------------------------------------------------------------------------------
--                         Pre and Postfix Operators                          --
--------------------------------------------------------------------------------

preop :: Parser PrefixOperator
preop =  Predecrement <$ symbol "--"
     <|> Preincrement <$ symbol "++"
     <|> Dereference  <$ symbol "*"
     <|> AddressOf    <$ symbol "&"
     <|> Not          <$ symbol "!"
     <|> Complement   <$ symbol "~"
     <|> UnaryPlus    <$ symbol "+"
     <|> UnaryMinus   <$ symbol "-"
     <|> CCast        <$> withinParens typename -- eg (int)x

postop :: Parser PostfixOperator
postop =  Postdecrement <$ symbol "--"
      <|> Postincrement <$ symbol "++"
      <|> Call          <$> withinParens   (expression `sepBy` comma)
      <|> ArrayAccess   <$> withinBrackets expression
      <|> MemberAccess  <$> some members
  where
    members :: Parser Member
    members =  Mdot    <$ (char   '.' ) <*> identifier
           <|> Mptr    <$ (string "->") <*> identifier
           <|> Mstatic <$ (string "::") <*> identifier

--------------------------------------------------------------------------------
--                              Identifiers                                   --
--------------------------------------------------------------------------------

_keywords :: [String]
-- :TODO: reintroduce primitive type keywords once typeparsing is done
--_keywords = [
--  "asm", "auto", "bool", "break", "case", "catch", "class", "const",
--  "const_cast", "continue", "default", "delete", "do", "double", "dynamic_cast",
--  "else", "enum", "explicit", "extern", "float", "for", "goto", "inline", "int",
--  "namespace", "new", "operator", "private", "public", "reinterpret_cast",
--  "signed", "sizeof", "static_cast", "struct", "template", "throw", "try",
--  "typeid", "union", "unsigned", "virtual", "volatile", "while"
--  ]
_keywords = [
  "asm", "auto", "break", "case", "catch", "class", "const",
  "const_cast", "continue", "default", "delete", "do", "dynamic_cast",
  "else", "enum", "explicit", "extern", "for", "goto", "inline",
  "namespace", "new", "operator", "private", "public", "reinterpret_cast",
  "sizeof", "static_cast", "struct", "template", "throw", "try",
  "typeid", "union", "virtual", "volatile", "while"
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
typename :: Parser String
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

_varInitializer :: Parser (Maybe Expression)
_varInitializer =  Nothing <$ notFollowedBy (char '=')
               <|> Just <$ symbol "=" <*> expression

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
declVariable :: Parser [Declaration]
declVariable = do
  -- Parse the "common" prefix, IE: qualifiers and typename
  qualifiers <- many _qualifier
  tname      <- typename
  -- Parse the set of fields, seperated by ','
  vardecls <- (sepBy (vardecl qualifiers tname) comma)
  return vardecls
  where
    vardecl :: [Qualifier] -> Type -> Parser Declaration
    vardecl q t = (DeclVar q t) <$> _indirection <*> identifier <*> _varStorage <*> _varInitializer

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
literalInt = LiteralInt <$> L.decimal


-- _literalFloating :: (a -> Literal) -> Char -> Parser Float
_literalFloating litType c =
      try ((litType . realToFrac  ) <$> pfloat    <* end)
  <|> try ((litType . fromIntegral) <$> L.decimal <* (char '.' <* notFollowedBy digitChar) <* end)
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

literalNullptr :: Parser Literal
literalNullptr = LiteralNullptr <$ keyword "nullptr"

literal :: Parser Literal
literal  =  lexeme (choice [ literalNullptr
                           , literalString
                           , literalChar
                           , literalDouble
                           , literalFloat
                           , literalInt
                           ]
                   )
--------------------------------------------------------------------------------
--                                 Expressions                                --
--------------------------------------------------------------------------------

-- Expression parsing with only guarded recursion preventing infinite
-- recursive behaviour without any terms being consumed
_exprTerm :: Parser Expression
_exprTerm = build <$> term <*> many postop
  where
    term :: Parser Expression
    term =  try (ExprLiteral    <$> literal)
        <|> try (ExprIdentifier <$> identifier)
        <|> try (ExprPrefix     <$> preop <*> _exprTerm)
        <|> try (withinParens expression)
    build :: Expression -> [PostfixOperator] -> Expression
    build e []        = e
    build e (op:rest) = build (ExprPostfix e op) rest

-- Sets of operators of equal precedence from most to least
-- see: https://en.cppreference.com/w/cpp/language/operator_precedence
_exprOpPrecedence = [ [ OpMul, OpDiv, OpMod ]
                    , [ OpAdd, OpSub ]
                    , [ OpShl, OpShr ]
                    , [ OpLt, OpGt, OpLe, OpGe ]
                    , [ OpEq, OpNeq ]
                    , [ OpBitAnd ]
                    , [ OpBitXor ]
                    , [ OpBitOr ]
                    , [ OpAnd ]
                    , [ OpOr ]
                    , [ OpAssign AssignEq
                      , OpAssign AssignAdd,    OpAssign AssignSub
                      , OpAssign AssignMul,    OpAssign AssignDiv
                      , OpAssign AssignBitAnd, OpAssign AssignBitOr
                      , OpAssign AssignBitXor
                      , OpAssign AssignAnd,  OpAssign AssignOr
                      , OpAssign AssignShl,  OpAssign AssignShr
                      ]
                    ]

expression :: Parser Expression
expression = (genout . (fullCollapse _exprOpPrecedence)) <$> plist
  where
    -- Parses a list of sub expressions joined by binary operators
    -- For example, 1 - 2 * 3 becomes:
    -- [(+, 1), (-, 2), (*, 3)]
    plist :: Parser [(BinaryOperator, Expression)]
    plist = ((:)
              <$> ((,) <$> produce OpAdd <*>  _exprTerm)
              <*> (many ((,) <$> binop <*> _exprTerm))
            )

    -- Collapses a list produced by plist by joining neighbouring
    -- terms where the operator is within some set, this enables collapsing
    -- multiplication before addition and so on
    --  - cset :: [BinaryOperator]                -> Set to collapse
    --  - list :: [[(BinaryOperator, Expression)] -> Input list
    collapse :: [BinaryOperator] -> [(BinaryOperator, Expression)] -> [(BinaryOperator, Expression)]
    collapse cset ((op1, expr1) : (op2, expr2) : rest) =
      if op2 `elem` cset
      then collapse cset ((op1, (ExprBinary expr1 op2 expr2)) : rest)
      else (op1, expr1) : (collapse cset ((op2, expr2) : rest))
    collapse cset (head : []) = [head]

    -- fully collapses an expression list by calling collapse multiple times for
    -- each set of operators in a list of operator precedences
    fullCollapse :: [[BinaryOperator]] -> [(BinaryOperator, Expression)] -> [(BinaryOperator, Expression)]
    fullCollapse []   elist  = elist -- Base case, stop when no more operators
    fullCollapse _    (e:[]) = [e]   -- Base case, bail out early if fully collapsed
    fullCollapse prec elist  = fullCollapse (tail prec) (collapse (head prec) elist)

    -- Takes the single term output after collapse has been applied for all
    -- operators, strips the leading dummy + operator, and returns the
    -- produced expression
    genout :: [(BinaryOperator, Expression)] -> Expression
    genout ((OpAdd, expr) : []) = expr

--------------------------------------------------------------------------------
--                            Type System                                     --
--------------------------------------------------------------------------------

-- data Typeid = Type  String          -- Base type,         eg, int
--             | Tmem  Typeid Typeid   -- Type member,       eg, xen::Window
--             | Tinst Typeid [TParam] -- Template instance, eg, Vec<3,float>

_typeid_guarded :: Parser Typeid
_typeid_guarded =  try (Type <$> integral)
               <|> try (Type <$> keyword "bool")
               <|> try (Type <$> keyword "float")
               <|> try (Type <$> keyword "double")
               <|> try (buildTinst <$> identifier <*> tparamlist)
  where
    integral :: Parser String
    integral = (intercalate " ") <$> some (keyword "unsigned" <|>
                                           keyword "signed"   <|>
                                           keyword "long"     <|>
                                           keyword "int"
                                          )
    buildTinst :: String -> Maybe [TParam] -> Typeid
    buildTinst id Nothing   = Type id
    buildTinst id (Just tp) = (Tinst (Type id) tp)

    -- Tparam parsing is somwhat messy...
    -- Issue is that its fairly ambigious as <x> may be x as a
    -- type, or a constexpr
    --
    -- Nievely we might try (sepBy tparam comma)
    -- Where tparam first tries to parse a type, then an expression
    --
    -- But now consider trying to parse "<x+y>". This parses the "x" as
    -- a type, then fails to find a "," (for more tparams) or ">" (to end
    -- the tparam list). At that point we fail the entire template list parse
    -- causing parse error to be produced.
    --
    -- If we define ~tparam~ to try expression then type then <x> fails
    -- as we assume by default tparams are types rather than expressions
    --
    -- Hence instead we must define ~tparam~ to include the terminating character
    -- so that we backtrack inside of the ~tparam~ parser such as in the case of
    -- <x+y>
    tparamlist :: Parser (Maybe [TParam])
    tparamlist = Just <$ symbol "<" <*> ( (++)
                                          <$> (many (tparam comma))
                                          <*> ((\x -> x:[]) <$> (tparam (symbol ">")))
                                        )
              <|> produce Nothing
    tparam :: Parser a -> Parser TParam
    tparam term =  try (TParamType <$> qtype      <* term)
               <|> try (TParamExpr <$> expression <* term)


typeid :: Parser Typeid
typeid =  try (Tmem <$> _typeid_guarded <* string "::" <*> typeid)
      <|> _typeid_guarded
  where
    integral :: Parser String
    integral = (intercalate " ") <$> some (keyword "unsigned" <|>
                                           keyword "signed"   <|>
                                           keyword "long"     <|>
                                           keyword "int"
                                          )

-- Parses parts of a qualified type that come before the variable name
-- IE: const int x* thing[3];
--     ------------
-- This hence ignores Qarray, Qflexarray and Qbitfield
-- since these specifiers must all follow the variable name
_qtypeBeforeName :: Parser QType
_qtypeBeforeName = do
  quals    <- many qualifier
  typename <- (QType <$> typeid)
  indirect <- many indirection
  return (foldr ($) typename ((reverse indirect) ++ quals))
  where
    qualifier :: Parser (QType -> QType)
    qualifier = choice [ QConst     <$ keyword "const"
                       , QConstexpr <$ keyword "constexpr"
                       , QStatic    <$ keyword "static"
                       , QVolatile  <$ keyword "volatile"
                       , QMutable   <$ keyword "mutable"
                       ]

    indirection :: Parser (QType -> QType)
    indirection = choice [ try( Qconstptr <$ symbol "*" <* keyword "const" )
                         ,      Qptr      <$ symbol "*"
                         ,      Qref      <$ symbol "&"
                         ]

-- Parses parts of a qualified type that come after the variable name
-- IE: const int x* thing[3];
--                       ---
-- Takes as input the portion before the variable name in order to
-- construct the full QType
_qtypeAfterName :: QType -> Parser QType
_qtypeAfterName qt = foldr ($) qt
  <$> choice [ (:[]) <$> (Qbitfield <$ symbol ":" <*> expression)
             , many (choice [ try (Qflexarray <$ symbol "[]")
                            , Qarray <$> withinBrackets expression
                            ]
                    )
             ]

qtypeWithName :: Parser (QType, Identifier)
qtypeWithName = do
  prename <- _qtypeBeforeName
  name    <- identifier
  qt      <- _qtypeAfterName prename
  return (qt, name)

qtype :: Parser QType
qtype = _qtypeBeforeName >>= _qtypeAfterName
