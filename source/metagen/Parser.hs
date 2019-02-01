module Parser where

import Ast

import Prelude hiding (const)
import Data.Void
import Data.List
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
     <|> CCast        <$> withinParens qtype -- eg (int)x

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

--------------------------------------------------------------------------------
--                             Declarations                                   --
--------------------------------------------------------------------------------

-- Parses a single "line" of struct field definitions
--
-- In the simple case this may just be "int x"
--
-- "Indirection" is used to represent pointers and references, such as:
-- "int** const x
--
-- This also supports multiple field definitions, eg:
-- int x, *ptr
--
-- Note that the following is valid (but extreamly ugly) c++:
-- int& thing, * const test, array[5];
declVariable :: Parser [Declaration]
declVariable = do
  -- Parse the "common" prefix, IE: qualifiers and typename
  qualifiers <- qtype_qualifiers
  tid        <- (QType <$> typeid)
  -- Parse the set of fields, seperated by ','
  vardecls <- (sepBy1 (vardecl (qualifiers tid)) comma)
  return vardecls
  where
    varInitializer :: Parser (Maybe Expression)
    varInitializer =  Nothing <$ notFollowedBy (char '=')
                  <|> Just <$ symbol "=" <*> expression

    vardecl :: QType -> Parser Declaration
    vardecl base_type = do
      indir       <- qtype_indirection
      varname     <- identifier
      storage     <- qtype_storage
      initializer <- varInitializer
      return (DeclVar ((storage . indir) base_type) varname initializer)

accessModifier :: Parser AccessModifier
accessModifier = choice [ Public    <$ keyword "public"
                        , Private   <$ keyword "private"
                        , Protected <$ keyword "protected"
                        ]

defaultAccessModifier :: CompositeKind -> AccessModifier
defaultAccessModifier Struct = Public
defaultAccessModifier Union  = Public
defaultAccessModifier Class  = Private

declType :: Parser Declaration
declType = do
  default_am <- defaultAccessModifier <$> (Class <$ keyword "class" <|> Struct <$ keyword "struct")
  name       <- (option "" identifier)
  parents    <- (option [] (inheritList default_am))
  members    <- withinBraces (body default_am)
  return (DeclType name parents members)
  where
    inheritList :: AccessModifier -> Parser [Inherit]
    inheritList default_am = symbol ":" *> sepBy1 ((,) <$> (option default_am accessModifier)
                                                       <*> typeid
                                                  ) comma

    body :: AccessModifier -> Parser [TypeMember]
    body default_am = (foldr (++) []) <$> ((:)
                                           <$> ((foldr (++) []) <$>(many (bodyStmt default_am)))
                                           <*> many (bodyBlock default_am)
                                          )

    -- Parses a block of the body optionally prefixed with
    -- some access modifier statement
    bodyBlock :: AccessModifier -> Parser [TypeMember]
    bodyBlock default_am = do
      am    <- (accessModifier <* symbol ":")
      block <- many (bodyStmt am)
      return (foldr (++) [] block)

    -- Parses a single statement in the body
    bodyStmt :: AccessModifier -> Parser [TypeMember]
    bodyStmt default_am =
      (choice
        [ ((:[]) . ((,) default_am)) <$> declType
        , ((:[]) . ((,) default_am)) <$> declUnion
        , map ((,) default_am)       <$> declVariable
          -- :TOOD: declFunction
        ]
      ) <* symbol ";"

declUnion :: Parser Declaration
declUnion = DeclUnion <$ keyword "union" <*> identifier <*> withinBraces body
  where
    body :: Parser [Declaration]
    body = foldr (++) [] <$> many (choice [ (:[]) <$> declType
                                          , (:[]) <$> declUnion
                                          , declVariable
                                          ]
                                  )

--------------------------------------------------------------------------------
--                                 Literals                                   --
--------------------------------------------------------------------------------

-- Parses a sequence of characters quoted by some other, where the sequence
-- may include the quote character if it is prefixed by \
_quotedSeq :: Char -> (Parser String -> Parser [String]) -> Parser String
_quotedSeq c content =
  (concat) <$> between (char c) (char c) (content (escaped <|> nonCloser))
  where
    escaped   = try( string ('\\' : c : []))
    nonCloser = (:) <$> satisfy (/=c) <*> produce []

literalString :: Parser Literal
literalString = LString <$> _quotedSeq '"' many

literalChar :: Parser Literal
literalChar = LChar <$> _quotedSeq '\'' some

literalInt :: Parser Literal
literalInt = LInt <$> L.decimal


_literalFloating :: Fractional b => (b -> a) -> Char -> Parser a
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
literalDouble = _literalFloating LDouble 'd'
literalFloat  :: Parser Literal
literalFloat  = _literalFloating LFloat 'f'

literalArray :: Parser Literal
literalArray = LArray <$> withinBrackets (sepBy1 expression comma)

literalInitList :: Parser Literal
literalInitList = LInitList <$> withinBraces (sepBy1 expression comma)

literalNullptr :: Parser Literal
literalNullptr = LNullptr <$ keyword "nullptr"

literal :: Parser Literal
literal  =  lexeme (choice [ literalNullptr
                           , literalArray
                           , literalInitList
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
_exprOpPrecedence :: [[BinaryOperator]]
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
    collapse _ (x : []) = [x]
    collapse _ []       = error "Attempt to collapse empty expression list"

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
    genout _ = error "Collapse resulted in bad output"

--------------------------------------------------------------------------------
--                            Type System                                     --
--------------------------------------------------------------------------------

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
    buildTinst typename Nothing   = Type typename
    buildTinst typename (Just tp) = (Tinst (Type typename) tp)

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

-- Parses arbitrary number of qualifiers before a typename
-- Eg, "const static"
--
-- Returns a function which, when given a QType wraps it within
-- the qualifiers
qtype_qualifiers :: Parser (QType -> QType)
qtype_qualifiers = (foldr (.) id) <$>
  -- Each of the data constructors below have type (QType -> QType)
  -- The foldr combines these with function composition (IE: (.))
  -- starting at the base case of ~id~ such that the overall result
  -- still has the type (QType -> QType)
  many (choice [ QConst     <$ keyword "const"
               , QConstexpr <$ keyword "constexpr"
               , QStatic    <$ keyword "static"
               , QVolatile  <$ keyword "volatile"
               , QMutable   <$ keyword "mutable"
               ]
       )

-- Parses an "indirection specifier", IE: set of pointers or references
-- that occur AFTER a typeid
-- for example, would parse the "** const" in "int** const"
--
-- Returns a function which, given a QType, wraps it within the
-- indirection specifiers
qtype_indirection :: Parser (QType -> QType)
qtype_indirection = ((foldr (.) id) . reverse) <$>
  many ( choice [ try( Qconstptr <$ symbol "*" <* keyword "const" )
                ,      Qptr      <$ symbol "*"
                ,      Qref      <$ symbol "&"
                ]
       )

-- Parses an array specifier for a qualified type
-- For example, would parse the ~[4][4]~ in ~int x[4][4]~
--
-- Yields a function which, given a QType, wraps it in the array
-- specifier to construct the full QType
--
-- Note that this parser will return the function "id" and consume
-- no tokens if there is no array specifier at the current location
qtype_array :: Parser (QType -> QType)
qtype_array = (foldr (.) id) <$>
  many (choice [ try (Qflexarray <$ symbol "[]")
               , Qarray <$> withinBrackets expression
               ]
       )

-- Parses a bitfield specifier, IE: ~: 3~ in ~int x : 3~
-- Parser will fail if there is no bitfield specifier at
-- the current location
--
-- Yields a function which, given a QType, wraps it in the Qbitfield
-- specifier to construct the full QType
qtype_bitfield :: Parser (QType -> QType)
qtype_bitfield = Qbitfield <$ symbol ":" <*> expression

-- Parses qualified type storage specification, IE: either an array
-- specifier or bitfield specifier
-- Both of these follow the typename (eg, int x[3], int x : 3)
--
-- Yields a function which, given a QType, wraps it in the qualifiers
-- to construct the full QType
--
-- This parser will return id if there is no specifier at the current location
qtype_storage :: Parser (QType -> QType)
qtype_storage = qtype_bitfield <|> qtype_array

-- Parses a bare qualfied type without a variable name
-- where the type is refering to an object (IE: any type
-- except a function pointer)
qtype_object :: Parser QType
-- EG, for ~const static int*[3]~
qtype_object = do
  quals    <- qtype_qualifiers    --- const static
  typename <- (QType <$> typeid)  --- int
  indirect <- qtype_indirection   --- *
  storage  <- qtype_storage       --- [3]
  return ((storage . indirect . quals) typename)

-- Parses a bare qualified type without a variable name
-- For example, a type argument to a template
qtype :: Parser QType
qtype = qtype_object -- :TODO: Also parse function pointers...
