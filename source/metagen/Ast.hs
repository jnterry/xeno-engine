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


data StructField =
  --          qualifiers  Type     Pointer?    Varname
  StructField [Qualifier] Typename Indirection String
  deriving (Show)
