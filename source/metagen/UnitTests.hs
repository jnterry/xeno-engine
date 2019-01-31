module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Parser
import Ast

--------------------------------------------------------------------------------
--                                  Main                                      --
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  suite_binop
  suite_literal
  suite_declvar
  suite_expression

--------------------------------------------------------------------------------
--                                Helpers                                     --
--------------------------------------------------------------------------------

itShouldParse parser input expected = it input $ do
  parse parser "" input `shouldParse` expected

itShouldFail parser input = it input $ do
  parse parser "" `shouldFailOn` input


--------------------------------------------------------------------------------
--                             Test Suites                                    --
--------------------------------------------------------------------------------

suite_binop = describe "binop" $ do
  pass "+"   (OpAdd)
  pass "-"   (OpSub)
  pass "*"   (OpMul)
  pass "/"   (OpDiv)
  pass "%"   (OpMod)
  pass ">>=" (OpAssign AssignShl)
  pass ">>"  (OpShl)
  pass ">"   (OpGt)
  pass ">="  (OpGe)
  pass "=="  (OpEq)
  pass "!="  (OpNeq)
  pass "="   (OpAssign AssignEq)

  fail "/*"
  fail "<>"
  fail "!>"
  fail "==="
  fail ">>>"
  fail "a"
  fail ""
  where
    pass input output = itShouldParse (binop <* eof) input output
    fail input        = itShouldFail  (binop <* eof) input

--------------------------------------------------------------------------------

suite_literal = describe "literal" $ do
  fail   ""

  pass "nullptr" LiteralNullptr

  pass "'a'"    (LiteralChar   ("a"    ))
  pass "'ab'"   (LiteralChar   ("ab"   ))
  pass "'\\\''" (LiteralChar   ("\\\'" ))
  pass "'\\0'"  (LiteralChar   ("\\0"  ))
  fail   "'"
  fail   "'''"
  fail   "''"

  pass "\"HELLO\""    (LiteralString   ("HELLO"))
  pass "\"\""         (LiteralString   (""))
  pass "\"'\""        (LiteralString   ("'"))
  pass "\"\\\"\""     (LiteralString   ("\\\""))
  fail   "\"\"\""
  fail   "\"hi\"\""

  pass "123"  (LiteralInt    (  123 ))
  pass "0.5"  (LiteralDouble (   0.5))
  pass "0.5f" (LiteralFloat  (   0.5))
  pass "0.5d" (LiteralDouble (   0.5))
  pass "5."   (LiteralDouble (   5.0))
  pass "5.f"  (LiteralFloat  (   5.0))
  pass "5.d"  (LiteralDouble (   5.0))
  pass ".5"   (LiteralDouble (   0.5))
  pass ".5f"  (LiteralFloat  (   0.5))
  pass ".5d"  (LiteralDouble (   0.5))

  -- This handled by unary plus/minus expression
  fail  "-5"
  fail  "- 5"
  fail  "- 1.2"
  where
    pass input output = itShouldParse (literal <* eof) input output
    fail   input        = itShouldFail  (literal <* eof) input

--------------------------------------------------------------------------------

suite_expression = describe "expression" $ do
  suite_expr_simple
  suite_expr_prefix
  suite_expr_postfix
  suite_expr_precedence

suite_expr_simple = describe "simple" $ do
  fail ""
  pass "5"      (ExprLiteral    (LiteralInt 5))
  pass "hello"  (ExprIdentifier "hello")
  pass "5 + 6"  (ExprBinary  (ExprLiteral (LiteralInt 5)) OpAdd    (ExprLiteral (LiteralInt 6)))
  pass "5 & 6"  (ExprBinary  (ExprLiteral (LiteralInt 5)) OpBitAnd (ExprLiteral (LiteralInt 6)))
  pass "5 == 6" (ExprBinary  (ExprLiteral (LiteralInt 5)) OpEq    (ExprLiteral (LiteralInt 6)))
  where
    pass input output = itShouldParse (expression <* eof) input output
    fail input        = itShouldFail  (expression <* eof) input

suite_expr_prefix = describe "prefix" $ do
  pass "-2"            (ExprPrefix UnaryMinus  (ExprLiteral (LiteralInt   2  )))
  pass "+1.0f"         (ExprPrefix UnaryPlus   (ExprLiteral (LiteralFloat 1.0)))
  pass "*a"            (ExprPrefix Dereference (ExprIdentifier "a"))
  pass "&a"            (ExprPrefix AddressOf   (ExprIdentifier "a"))
  pass "!a"            (ExprPrefix Not         (ExprIdentifier "a"))
  pass "~a"            (ExprPrefix Complement  (ExprIdentifier "a"))
  pass "**a"           (ExprPrefix Dereference ((ExprPrefix Dereference (ExprIdentifier "a"))))
  pass "(xen::Vec2r)a" (ExprPrefix (CCast "xen::Vec2r") (ExprIdentifier "a"))

  pass "--a"   (ExprPrefix Predecrement  (ExprIdentifier "a"))
  pass "- -a"  (ExprPrefix UnaryMinus (ExprPrefix UnaryMinus (ExprIdentifier "a")))
  pass "- - a" (ExprPrefix UnaryMinus (ExprPrefix UnaryMinus (ExprIdentifier "a")))
  pass "- + a" (ExprPrefix UnaryMinus (ExprPrefix UnaryPlus  (ExprIdentifier "a")))
  pass "-+ a"  (ExprPrefix UnaryMinus (ExprPrefix UnaryPlus  (ExprIdentifier "a")))
  pass "-+a"   (ExprPrefix UnaryMinus (ExprPrefix UnaryPlus  (ExprIdentifier "a")))

  -- Unary operators have higher precedence than binary operators
  pass "- -a + -b" (ExprBinary
                    (ExprPrefix UnaryMinus (ExprPrefix UnaryMinus (ExprIdentifier "a")))
                    OpAdd
                    (ExprPrefix UnaryMinus (ExprIdentifier "b"))
                   )
  where
    pass input output = itShouldParse (expression <* eof) input output
    fail input        = itShouldFail  (expression <* eof) input

suite_expr_postfix = describe "postfix" $ do
  pass "a++" (ExprPostfix (ExprIdentifier "a") Postincrement)
  pass "a--" (ExprPostfix (ExprIdentifier "a") Postdecrement)

  pass "a()"            (ExprPostfix (ExprIdentifier "a")
                         (Call []))
  pass "a(b)"           (ExprPostfix (ExprIdentifier "a")
                         (Call [ExprIdentifier "b"]))
  pass "a(b,c)"         (ExprPostfix (ExprIdentifier "a")
                         (Call [ExprIdentifier "b", ExprIdentifier "c"]))
  pass "a(1+b, (int)x)" (ExprPostfix (ExprIdentifier "a")
                         (Call [ ExprBinary (ExprLiteral (LiteralInt 1)) OpAdd (ExprIdentifier "b")
                               , ExprPrefix (CCast "int") (ExprIdentifier "x")
                               ]
                         ))

  pass "a[i]"    (ExprPostfix (ExprIdentifier "a") (ArrayAccess (ExprIdentifier "i")))
  pass "a(b)[0]" (ExprPostfix
                  (ExprPostfix (ExprIdentifier "a") (Call [(ExprIdentifier "b")]))
                  (ArrayAccess (ExprLiteral (LiteralInt 0)))
                 )

  where
    pass input output = itShouldParse (expression <* eof) input output
    fail input        = itShouldFail  (expression <* eof) input

suite_expr_precedence = describe "precedence" $ do
  -- :TODO: Note that we always use left-to-right associativity
  -- IE: 1 - 2 - 3 is equivalent to (1-2)-3
  --
  -- Some operators (assignment) use right-to-left as per C spec, eg:
  -- x = y = z is x = (y = z)
  --
  -- See: https://en.cppreference.com/w/cpp/language/operator_precedence
  --
  -- Our parser DOES NOT obey this!
  -- (but precedence is correctly handled, eg, * always before +)

  -- Arithmetic precedence
  pass "1 + 2 + 3" (ExprBinary
                     (ExprBinary (ExprLiteral (LiteralInt 1)) OpAdd (ExprLiteral (LiteralInt 2)))
                     OpAdd
                     (ExprLiteral (LiteralInt 3))
                   )
  pass "1 * 2 + 3" (ExprBinary
                     (ExprBinary (ExprLiteral (LiteralInt 1)) OpMul (ExprLiteral (LiteralInt 2)))
                     OpAdd
                     (ExprLiteral (LiteralInt 3))
                   )
  pass "1 - 2 / 3" (ExprBinary
                     (ExprLiteral (LiteralInt 1))
                     OpSub
                     (ExprBinary (ExprLiteral (LiteralInt 2)) OpDiv (ExprLiteral (LiteralInt 3)))
                   )
  -- Logical precedence
  pass "a || b || c" (ExprBinary
                      (ExprBinary (ExprIdentifier "a") OpOr (ExprIdentifier "b"))
                      OpOr
                      (ExprIdentifier "c")
                     )
  pass "a && b || c" (ExprBinary
                      (ExprBinary (ExprIdentifier "a") OpAnd (ExprIdentifier "b"))
                       OpOr
                       (ExprIdentifier "c")
                     )
  pass "a || b && c" (ExprBinary
                       (ExprIdentifier "a")
                       OpOr
                      (ExprBinary (ExprIdentifier "b") OpAnd (ExprIdentifier "c"))
                     )

  -- Bitwise precedence
  pass "a & b ^ c | d" (ExprBinary
                         (ExprBinary
                          (ExprBinary (ExprIdentifier "a") OpBitAnd (ExprIdentifier "b"))
                          OpBitXor
                          (ExprIdentifier "c")
                         )
                         OpBitOr
                         (ExprIdentifier "d")
                       )
  pass "a ^ b & c | d" (ExprBinary
                         (ExprBinary
                           (ExprIdentifier "a")
                           OpBitXor
                           (ExprBinary (ExprIdentifier "b") OpBitAnd (ExprIdentifier "c"))
                         )
                         OpBitOr
                         (ExprIdentifier "d")
                       )
  pass "a | b & c ^ d" (ExprBinary
                         (ExprIdentifier "a")
                         OpBitOr
                         (ExprBinary
                           (ExprBinary (ExprIdentifier "b") OpBitAnd (ExprIdentifier "c"))
                           OpBitXor
                           (ExprIdentifier "d")
                         )
                       )

  -- Mixed precedence
  pass "a & b + c" (ExprBinary
                     (ExprIdentifier "a")
                     OpBitAnd
                     (ExprBinary (ExprIdentifier "b") OpAdd (ExprIdentifier "c"))
                   )
  pass "a || b << c" (ExprBinary
                      (ExprIdentifier "a")
                      OpOr
                      (ExprBinary (ExprIdentifier "b") OpShl (ExprIdentifier "c"))
                     )
  pass "a && b == c" (ExprBinary
                      (ExprIdentifier "a")
                       OpAnd
                       (ExprBinary (ExprIdentifier "b") OpEq (ExprIdentifier "c"))
                     )
  pass "a == b + c" (ExprBinary
                      (ExprIdentifier "a")
                       OpEq
                      (ExprBinary (ExprIdentifier "b") OpAdd (ExprIdentifier "c"))
                    )
  where
    pass input output = itShouldParse (expression <* eof) input output
    fail input        = itShouldFail  (expression <* eof) input

--------------------------------------------------------------------------------

suite_declvar = describe "declVariable" $ do
  pass "int x"        [(DeclVar [      ] "int" Direct "x" Standalone)      Nothing]
  pass "int x[3]"     [(DeclVar [      ] "int" Direct "x" (FixedArray 3))  Nothing]
  pass "int x[]"      [(DeclVar [      ] "int" Direct "x" (FlexibleArray)) Nothing]
  pass "int x : 3"    [(DeclVar [      ] "int" Direct "x" (Bitfield 3))    Nothing]
  pass "const int x"  [(DeclVar [Const ] "int" Direct "x" Standalone)      Nothing]
  pass "static int x" [(DeclVar [Static] "int" Direct "x" Standalone)      Nothing]
  pass "int x = 5"    [(DeclVar [      ] "int" Direct "x" Standalone)
                        (Just (ExprLiteral (LiteralInt 5)))
                      ]
  pass "static constexpr bool thing" [(DeclVar
                                        [Static, Constexpr]
                                        "bool" Direct "thing" Standalone Nothing)
                                     ]
  pass "xen::Aabb2r x"               [(DeclVar
                                        [] "xen::Aabb2r" Direct "x" Standalone Nothing)
                                     ]
  pass "u64* z"                      [(DeclVar
                                        [] "u64" (Pointer 1 False) "z" Standalone Nothing)
                                     ]
  pass "u64** z"                     [(DeclVar
                                        [] "u64" (Pointer 2 False) "z" Standalone Nothing)
                                     ]
  pass "u64** const z"               [(DeclVar
                                        [] "u64" (Pointer 2 True) "z" Standalone Nothing)
                                     ]
  pass "int x, y"         [(DeclVar [] "int" Direct "x" Standalone Nothing)
                          ,(DeclVar [] "int" Direct "y" Standalone Nothing)]
  pass "const int x, y"   [(DeclVar [Const] "int" Direct "x" Standalone Nothing)
                          ,(DeclVar [Const] "int" Direct "y" Standalone Nothing)]
  pass "const int x, *y"  [(DeclVar [Const] "int" Direct "x" Standalone Nothing)
                          ,(DeclVar [Const] "int" (Pointer 1 False) "y" Standalone Nothing)]

  -- Yes, the following is valid c++, grim, but valid
  -- (at least inside a struct, since bitfields only work in structs)
  pass "const static int& ref, *const ptr = nullptr, **ptr2, fix[5], bit : 3, flag:1, flex[]"
    [ (DeclVar [Const, Static] "int" Reference         "ref"  Standalone Nothing)
    , (DeclVar [Const, Static] "int" (Pointer 1 True ) "ptr"  Standalone
        (Just (ExprLiteral LiteralNullptr)))
    , (DeclVar [Const, Static] "int" (Pointer 2 False) "ptr2" Standalone Nothing)
    , (DeclVar [Const, Static] "int" Direct            "fix"  (FixedArray 5) Nothing)
    , (DeclVar [Const, Static] "int" Direct            "bit"  (Bitfield 3) Nothing)
    , (DeclVar [Const, Static] "int" Direct            "flag" (Bitfield 1) Nothing)
    , (DeclVar [Const, Static] "int" Direct            "flex" (FlexibleArray) Nothing)
    ]
  where
    pass input output = itShouldParse (declVariable <* eof) input output
    fail input        = itShouldFail  (declVariable <* eof) input
