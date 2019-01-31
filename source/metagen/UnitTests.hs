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
  should "+"   (OpAdd)
  should "-"   (OpSub)
  should "*"   (OpMul)
  should "/"   (OpDiv)
  should "%"   (OpMod)
  should ">>=" (OpAssign AssignShl)
  fail   "a"
  fail   ""
  where
    should input output = itShouldParse (binop <* eof) input output
    fail   input        = itShouldFail  (binop <* eof) input

suite_literal = describe "literal" $ do
  fail   ""

  should "'a'"    (LiteralChar   ("a"    ))
  should "'ab'"   (LiteralChar   ("ab"   ))
  should "'\\\''" (LiteralChar   ("\\\'" ))
  should "'\\0'"  (LiteralChar   ("\\0"  ))
  fail   "'"
  fail   "'''"
  fail   "''"

  should "\"HELLO\""    (LiteralString   ("HELLO"))
  should "\"\""         (LiteralString   (""))
  should "\"'\""        (LiteralString   ("'"))
  should "\"\\\"\""     (LiteralString   ("\\\""))
  fail   "\"\"\""
  fail   "\"hi\"\""

  should "123"   (LiteralInt    (  123 ))
  should "-5"    (LiteralInt    (-  5  ))
  should "0.5"   (LiteralDouble (   0.5))
  should "0.5f"  (LiteralFloat  (   0.5))
  should "0.5d"  (LiteralDouble (   0.5))
  should "-5."   (LiteralDouble (-  5.0))
  should "-5.f"  (LiteralFloat  (-  5.0))
  should "-5.d"  (LiteralDouble (-  5.0))
  should "-.5"   (LiteralDouble (-0.5  ))
  should "-.5f"  (LiteralFloat  (-0.5  ))
  should "-.5d"  (LiteralDouble (-0.5  ))
  where
    should input output = itShouldParse (literal <* eof) input output
    fail   input        = itShouldFail  (literal <* eof) input

suite_declvar = describe "declVariable" $ do
  should "int x"        [(DeclVar [      ] "int" Direct "x" Standalone)     ]
  should "int x[3]"     [(DeclVar [      ] "int" Direct "x" (FixedArray 3)) ]
  should "int x[]"      [(DeclVar [      ] "int" Direct "x" (FlexibleArray))]
  should "int x : 3"    [(DeclVar [      ] "int" Direct "x" (Bitfield 3))   ]
  should "const int x"  [(DeclVar [Const ] "int" Direct "x" Standalone)     ]
  should "static int x" [(DeclVar [Static] "int" Direct "x" Standalone)     ]
  should "static constexpr bool thing" [(DeclVar
                                          [Static, Constexpr]
                                          "bool" Direct "thing" Standalone)
                                       ]
  should "xen::Aabb2r x"               [(DeclVar
                                         [] "xen::Aabb2r" Direct "x" Standalone)
                                       ]
  should "u64* z"                      [(DeclVar
                                         [] "u64" (Pointer 1 False) "z" Standalone)
                                       ]
  should "u64** z"                     [(DeclVar
                                         [] "u64" (Pointer 2 False) "z" Standalone)
                                       ]
  should "u64** const z"               [(DeclVar
                                          [] "u64" (Pointer 2 True) "z" Standalone)
                                       ]
  should "int x, y"         [(DeclVar [] "int" Direct "x" Standalone)
                            ,(DeclVar [] "int" Direct "y" Standalone)]
  should "const int x, y"   [(DeclVar [Const] "int" Direct "x" Standalone)
                            ,(DeclVar [Const] "int" Direct "y" Standalone)]
  should "const int x, *y"  [(DeclVar [Const] "int" Direct "x" Standalone)
                            ,(DeclVar [Const] "int" (Pointer 1 False) "y" Standalone)]

  -- Yes, the following is valid c++, grim, but valid
  -- (at least inside a struct, since bitfields only work in structs)
  should "const static int& ref, *const ptr, **ptr2, fix[5], bit : 3, flag:1, flex[]"
    [ (DeclVar [Const, Static] "int" Reference         "ref"  Standalone)
    , (DeclVar [Const, Static] "int" (Pointer 1 True ) "ptr"  Standalone)
    , (DeclVar [Const, Static] "int" (Pointer 2 False) "ptr2" Standalone)
    , (DeclVar [Const, Static] "int" Direct            "fix"  (FixedArray 5))
    , (DeclVar [Const, Static] "int" Direct            "bit"  (Bitfield 3))
    , (DeclVar [Const, Static] "int" Direct            "flag" (Bitfield 1))
    , (DeclVar [Const, Static] "int" Direct            "flex" (FlexibleArray))
    ]
  where
    should input output = itShouldParse (declVariable <* eof) input output
    fail   input        = itShouldFail  (declVariable <* eof) input
