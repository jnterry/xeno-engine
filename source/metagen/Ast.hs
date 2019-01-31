module Ast where

type Typename = String

-- Qualifier that may appear before a typename
-- eg, const int has qualifiers [Const]
data Qualifier = Const
               | Constexpr
               | Static
               | Volatile
               | Mutable
               deriving (Show, Eq)

-- Level of indirection for a variable
data Indirection = Direct           -- Plain variable, eg, char
                 | Pointer Int Bool -- Pointer, eg, char**, Int is number of *, Bool is if const is specified
                 | Reference        -- Reference, IE: char&
                 deriving (Show, Eq)

data VariableStorage =
    Standalone     -- The standard standalone variable, eg: int x;
  | FixedArray Int -- A fixed sized array, eg: int x[5];
  | FlexibleArray  -- An array with flexible length, eg: int x[];
  | Bitfield Int   -- Using only some bits of containing type, eg: int x : 3;
  deriving (Show, Eq)

type VariableName = String


data Decleration =
  --          qualifiers  Type     Pointer?    Varname
  VariableDeclaration [Qualifier] Typename Indirection VariableName VariableStorage
  deriving (Show, Eq)

data Literal = LiteralInt      Int
             | LiteralChar     String -- string since could be, eg, '\0'
             | LiteralString   String
             | LiteralFloat    Float
             | LiteralDouble   Double
             | LiteralArray    [Literal]
             | LiteralInitList [Literal]
             deriving (Show, Eq)
--data FunctionParam = FunctionParam Type VariableName (Maybe Literal)

-- Operators that can be used to express assignement, eg:
-- x  = 5
-- x += 6
data AssignmentOperator = AssignEq
                        | AssignAdd    | AssignSub   | AssignMul    | AssignDiv
                        | AssignBitAnd | AssignBitOr | AssignBitXor
                        | AssignAnd    | AssignOr -- no AssignXor, use != instead
                        | AssignShl    | AssignShr
                        deriving (Show, Eq)

-- Binary operators
data BinaryOperator = OpAdd | OpSub | OpMul | OpDiv | OpMod -- +   -   *   /   %
                    | OpBitAnd | OpBitOr | OpBitXor         -- &   |   ^
                    | OpAnd    | OpOr                       -- &&  ||
                    | OpShl    | OpShr                      -- <<  >>
                    | OpEq     | OpNeq                      -- ==  !=
                    | OpAssign AssignmentOperator -- eg, x = (a =* 5)
                    deriving (Show, Eq)


data Expression = ExprLiteral Literal
                deriving (Show, Eq)
