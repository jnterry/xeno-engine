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
  --suite_binop
  --suite_literal
  --suite_declvar
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
  fail   "a"
  fail   ""
  where
    pass input output = itShouldParse (binop <* eof) input output
    fail input        = itShouldFail  (binop <* eof) input

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

  pass "123"   (LiteralInt    (  123 ))
  pass "-5"    (LiteralInt    (-  5  ))
  pass "0.5"   (LiteralDouble (   0.5))
  pass "0.5f"  (LiteralFloat  (   0.5))
  pass "0.5d"  (LiteralDouble (   0.5))
  pass "-5."   (LiteralDouble (-  5.0))
  pass "-5.f"  (LiteralFloat  (-  5.0))
  pass "-5.d"  (LiteralDouble (-  5.0))
  pass "-.5"   (LiteralDouble (-0.5  ))
  pass "-.5f"  (LiteralFloat  (-0.5  ))
  pass "-.5d"  (LiteralDouble (-0.5  ))
  where
    pass input output = itShouldParse (literal <* eof) input output
    fail   input        = itShouldFail  (literal <* eof) input

suite_expression = describe "expression" $ do
  fail ""
  pass "5"     (ExprLiteral (LiteralInt 5))
  pass "5 + 6" (ExprBinary  (ExprLiteral (LiteralInt 5)) OpAdd (ExprLiteral (LiteralInt 6)))
  where
    pass input output = itShouldParse (expression <* eof) input output
    fail input        = itShouldFail  (expression <* eof) input


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
