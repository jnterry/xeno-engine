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
