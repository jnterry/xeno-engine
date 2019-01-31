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
  should "int x"                       [(VariableDeclaration
                                         [] "int" Direct "x" Standalone)
                                       ]
  should "int x[3]"                    [(VariableDeclaration
                                         [] "int" Direct "x" (FixedArray 3))
                                       ]
  should "int x[]"                     [(VariableDeclaration
                                         [] "int" Direct "x" (FlexibleArray))
                                       ]
  should "int x : 3"                   [(VariableDeclaration
                                         [] "int" Direct "x" (Bitfield 3))
                                       ]
  should "const int x"                 [(VariableDeclaration
                                         [Const] "int" Direct "x" Standalone)
                                       ]
  should "static int x"                [(VariableDeclaration
                                         [Static] "int" Direct "x" Standalone)
                                       ]
  should "static constexpr bool thing" [(VariableDeclaration
                                          [Static, Constexpr]
                                          "bool" Direct "thing" Standalone)
                                       ]
  should "xen::Aabb2r x"               [(VariableDeclaration
                                         [] "xen::Aabb2r" Direct "x" Standalone)
                                       ]
  should "u64* z"                      [(VariableDeclaration
                                         [] "u64" (Pointer 1 False) "z" Standalone)
                                       ]
  should "u64** z"                     [(VariableDeclaration
                                         [] "u64" (Pointer 2 False) "z" Standalone)
                                       ]
  should "u64** const z"               [(VariableDeclaration
                                          [] "u64" (Pointer 2 True) "z" Standalone)
                                       ]
  should "int x, y"         [(VariableDeclaration [] "int" Direct "x" Standalone)
                            ,(VariableDeclaration [] "int" Direct "y" Standalone)]
  should "const int x, y"   [(VariableDeclaration [Const] "int" Direct "x" Standalone)
                            ,(VariableDeclaration [Const] "int" Direct "y" Standalone)]
  should "const int x, *y"  [(VariableDeclaration [Const] "int" Direct "x" Standalone)
                            ,(VariableDeclaration [Const] "int" (Pointer 1 False) "y" Standalone)]

  -- Yes, the following is valid c++, grim, but valid
  -- (at least inside a struct, since bitfields only work in structs)
  should "const static int& ref, *const ptr, **ptr2, fix[5], bit : 3, flag:1, flex[]"
    [ (VariableDeclaration [Const, Static] "int" Reference         "ref"  Standalone)
    , (VariableDeclaration [Const, Static] "int" (Pointer 1 True ) "ptr"  Standalone)
    , (VariableDeclaration [Const, Static] "int" (Pointer 2 False) "ptr2" Standalone)
    , (VariableDeclaration [Const, Static] "int" Direct            "fix"  (FixedArray 5))
    , (VariableDeclaration [Const, Static] "int" Direct            "bit"  (Bitfield 3))
    , (VariableDeclaration [Const, Static] "int" Direct            "flag" (Bitfield 1))
    , (VariableDeclaration [Const, Static] "int" Direct            "flex" (FlexibleArray))
    ]
  where
    should input output = itShouldParse (declVariable <* eof) input output
    fail   input        = itShouldFail  (declVariable <* eof) input
