module Ast where

--------------------------------------------------------------------------------
--                              Expression                                    --
--------------------------------------------------------------------------------

data Literal = LInt      Int
             | LChar     String -- string since could be, eg, '\0'
             | LString   String
             | LFloat    Float
             | LDouble   Double
             | LNullptr
             | LArray    [Expression]
             | LInitList [Expression]
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
                    | OpLt   | OpGt | OpLe | OpGe           -- <   >   <=  >=
                    | OpAssign AssignmentOperator -- eg, x = (a =* 5)
                    deriving (Show, Eq)

data PrefixOperator = Predecrement | Preincrement -- --x  ++x
                    | Dereference  | AddressOf    --  *x   &x
                    | Not          | Complement   --  !x   ~x
                    | UnaryPlus    | UnaryMinus   --  +x   -x
                    | CCast QType                 -- (type)x
                    deriving (Show, Eq)

data Member = Mstatic Identifier -- ::x
            | Mptr    Identifier -- ->x
            | Mdot    Identifier --  .x
            deriving (Show, Eq)

data PostfixOperator = Postdecrement | Postincrement -- x--  x++
                     | Call        [Expression]      -- x(a,b)
                     | ArrayAccess Expression        -- x[1]
                     | MemberAccess [Member]         -- a->b, a.b, A::B
                     deriving (Show, Eq)

data Expression = ExprLiteral    Literal
                | ExprBinary     Expression BinaryOperator Expression
                | ExprIdentifier Identifier
                | ExprPrefix     PrefixOperator Expression
                | ExprPostfix    Expression PostfixOperator
                deriving (Show, Eq)

--------------------------------------------------------------------------------
--                              Type System                                   --
--------------------------------------------------------------------------------

-- Template parameter
-- May be either a type OR an expression
-- Eg, Vec<3, float>
data TParam = TParamType QType
            | TParamExpr Expression
            deriving (Show, Eq)

-- Represents some base type identifier
-- Eg: int
--     unsigned int
--     xen::Window
--     xen::Mat<4, 4, float>
data Typeid = Type  String          -- Base type,         eg, int
            | Tmem  Typeid Typeid   -- Type member,       eg, xen::Window
            | Tinst Typeid [TParam] -- Template instance, eg, Vec<3,float>
            deriving (Show, Eq)


-- Represents a type with additional qualifiers, for example
-- the Typeid "int" may be qualified to represent:
-- const int
-- int*
-- int**const *const
-- int[5]
-- int : 3
data QType = QType      Typeid  -- int
           | QConst     QType   -- const int
           | QConstexpr QType   -- constexpr int
           | QStatic    QType   -- static int
           | QVolatile  QType   -- volatile int
           | QMutable   QType   -- mutable int
           | Qptr       QType   -- int*
           | Qconstptr  QType   -- int* const
           | Qref       QType   -- int&
           | Qarray     Expression QType -- int[3]  (expression is contents of [])
           | Qbitfield  Expression QType -- int : 3 (expression is whats after :)
           | Qflexarray QType   -- int[]
           deriving (Show, Eq)

--------------------------------------------------------------------------------
--                             Declarations                                   --
--------------------------------------------------------------------------------

type Identifier = String

data AccessModifier  = Public | Protected | Private deriving (Show, Eq)
data CompositeKind   = Struct | Class     | Union   deriving (Show, Eq)

type Inherit    = (AccessModifier, Typeid     )
type TypeMember = (AccessModifier, Declaration)

data Declaration =
  --      Type  Varname     Optional Initializer
  DeclVar QType Identifier (Maybe Expression)

  --              Name   | Parent types | Member fields
  | DeclType  Identifier    [Inherit]     [ TypeMember]
  | DeclUnion Identifier                  [Declaration]
  deriving (Show, Eq)
