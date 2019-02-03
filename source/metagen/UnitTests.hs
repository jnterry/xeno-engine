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
  suite_expression
  suite_typeid
  suite_qtype
  suite_declvar
  suite_declunion
  suite_decltype

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

  pass "nullptr" LNullptr

  pass "'a'"    (LChar   ("a"    ))
  pass "'ab'"   (LChar   ("ab"   ))
  pass "'\\\''" (LChar   ("\\\'" ))
  pass "'\\0'"  (LChar   ("\\0"  ))
  fail   "'"
  fail   "'''"
  fail   "''"

  pass "\"HELLO\""    (LString   ("HELLO"))
  pass "\"\""         (LString   (""))
  pass "\"'\""        (LString   ("'"))
  pass "\"\\\"\""     (LString   ("\\\""))
  fail   "\"\"\""
  fail   "\"hi\"\""

  pass "123"  (LInt    (  123 ))
  pass "0.5"  (LDouble (   0.5))
  pass "0.5f" (LFloat  (   0.5))
  pass "0.5d" (LDouble (   0.5))
  pass "5."   (LDouble (   5.0))
  pass "5.f"  (LFloat  (   5.0))
  pass "5.d"  (LDouble (   5.0))
  pass ".5"   (LDouble (   0.5))
  pass ".5f"  (LFloat  (   0.5))
  pass ".5d"  (LDouble (   0.5))

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
  pass "5"      (ExprLiteral    (LInt 5))
  pass "hello"  (ExprIdentifier "hello")
  pass "5 + 6"  (ExprBinary  (ExprLiteral (LInt 5)) OpAdd    (ExprLiteral (LInt 6)))
  pass "5 & 6"  (ExprBinary  (ExprLiteral (LInt 5)) OpBitAnd (ExprLiteral (LInt 6)))
  pass "5 == 6" (ExprBinary  (ExprLiteral (LInt 5)) OpEq     (ExprLiteral (LInt 6)))
  where
    pass input output = itShouldParse (expression <* eof) input output
    fail input        = itShouldFail  (expression <* eof) input

suite_expr_prefix = describe "prefix" $ do
  pass "-2"            (ExprPrefix UnaryMinus  (ExprLiteral (LInt   2  )))
  pass "+1.0f"         (ExprPrefix UnaryPlus   (ExprLiteral (LFloat 1.0)))
  pass "*a"            (ExprPrefix Dereference (ExprIdentifier "a"))
  pass "&a"            (ExprPrefix AddressOf   (ExprIdentifier "a"))
  pass "!a"            (ExprPrefix Not         (ExprIdentifier "a"))
  pass "~a"            (ExprPrefix Complement  (ExprIdentifier "a"))
  pass "**a"           (ExprPrefix Dereference ((ExprPrefix Dereference (ExprIdentifier "a"))))
  pass "(xen::Vec2r)a" (ExprPrefix
                        (CCast (QType (Tmem (Type "xen") (Type "Vec2r"))))
                        (ExprIdentifier "a")
                       )

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
                         (Call [ ExprBinary (ExprLiteral (LInt 1)) OpAdd (ExprIdentifier "b")
                               , ExprPrefix (CCast (QType (Type "int"))) (ExprIdentifier "x")
                               ]
                         ))

  pass "a[i]"    (ExprPostfix (ExprIdentifier "a") (ArrayAccess (ExprIdentifier "i")))
  pass "a(b)[0]" (ExprPostfix
                  (ExprPostfix (ExprIdentifier "a") (Call [(ExprIdentifier "b")]))
                  (ArrayAccess (ExprLiteral (LInt 0)))
                 )

  pass "a.b"      (ExprPostfix (ExprIdentifier "a") (MemberAccess [Mdot    "b"]))
  pass "a->b"     (ExprPostfix (ExprIdentifier "a") (MemberAccess [Mptr    "b"]))
  pass "A::B"     (ExprPostfix (ExprIdentifier "A") (MemberAccess [Mstatic "B"]))

  pass "a(x).elems[0]" (ExprPostfix
                         (ExprPostfix
                          (ExprPostfix
                           (ExprIdentifier "a")
                           (Call [ExprIdentifier "x"])
                          )
                          (MemberAccess [Mdot "elems"])
                         )
                         (ArrayAccess (ExprLiteral (LInt 0)))
                       )

  pass "a(vec->x).hi.person[X::Y.z]"
    (ExprPostfix
      (ExprPostfix
        (ExprPostfix
          (ExprIdentifier "a")
          (Call [(ExprPostfix (ExprIdentifier "vec") (MemberAccess [Mptr "x"]))])
        )
        (MemberAccess [Mdot "hi", Mdot "person"])
      )
      (ArrayAccess (ExprPostfix (ExprIdentifier "X")
                    (MemberAccess [Mstatic "Y", Mdot "z"])
                   )
      )
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
                     (ExprBinary (ExprLiteral (LInt 1)) OpAdd (ExprLiteral (LInt 2)))
                     OpAdd
                     (ExprLiteral (LInt 3))
                   )
  pass "1 * 2 + 3" (ExprBinary
                     (ExprBinary (ExprLiteral (LInt 1)) OpMul (ExprLiteral (LInt 2)))
                     OpAdd
                     (ExprLiteral (LInt 3))
                   )
  pass "1 - 2 / 3" (ExprBinary
                     (ExprLiteral (LInt 1))
                     OpSub
                     (ExprBinary (ExprLiteral (LInt 2)) OpDiv (ExprLiteral (LInt 3)))
                   )
  pass "(1 - 2) / 3" (ExprBinary
                      (ExprBinary (ExprLiteral (LInt 1)) OpSub (ExprLiteral (LInt 2)))
                      OpDiv
                      (ExprLiteral (LInt 3))
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
  describe "failures" $ do
    fail ""
    fail "x"
    fail "int*"
    fail "int x*"
    fail "int x x"
    fail "int x const"

  describe "indirection" $ do
    pass "int  x"       [DeclVar (            QType (Type "int")  ) "x" Nothing]
    pass "int& x"       [DeclVar (Qref       (QType (Type "int")) ) "x" Nothing]
    pass "int* x"       [DeclVar (Qptr       (QType (Type "int")) ) "x" Nothing]
    pass "int* const x" [DeclVar (Qconstptr  (QType (Type "int")) ) "x" Nothing]
    pass "int** x"      [DeclVar (Qptr (Qptr (QType (Type "int")))) "x" Nothing]

  describe "qualifiers" $ do
    pass "const int x"        [DeclVar (        QConst  (QType (Type "int")) ) "x" Nothing]
    pass "static int x"       [DeclVar (        QStatic (QType (Type "int")) ) "x" Nothing]
    pass "const static int x" [DeclVar (QConst (QStatic (QType (Type "int")))) "x" Nothing]

  describe "storage types" $ do
    pass "int x[3]"         [DeclVar
                             (Qarray (ExprLiteral (LInt 3)) (QType (Type "int")))
                             "x" Nothing
                            ]
    pass "int x : 3"        [DeclVar
                             (Qbitfield (ExprLiteral (LInt 3)) (QType (Type "int")))
                             "x" Nothing
                            ]
    pass "unsigned int x[]" [DeclVar
                             (Qflexarray (QType (Type "unsigned int")))
                             "x" Nothing
                            ]
    pass "int x[3][5]"      [DeclVar
                             (Qarray
                              (ExprLiteral (LInt 3))
                              (Qarray (ExprLiteral (LInt 5)) (QType (Type "int")))
                             )
                             "x" Nothing
                            ]

  describe "initializers" $ do
    pass "int x = 5" [DeclVar (QType (Type "int")) "x"
                      (Just (ExprLiteral (LInt 5)))
                     ]

    pass "int x[] = [1,2,3]" [DeclVar (Qflexarray (QType (Type "int"))) "x"
                              (Just (ExprLiteral (LArray [ (ExprLiteral (LInt 1))
                                                         , (ExprLiteral (LInt 2))
                                                         , (ExprLiteral (LInt 3))
                                                         ]
                                                 )))
                             ]
    pass "xen::Vec2<const int> v = {5,6}" [ (DeclVar
                                             (QType (Tmem
                                                     (Type "xen")
                                                     (Tinst
                                                      (Type "Vec2")
                                                      [TParamType (QConst (QType (Type "int")))]
                                                     )))
                                             "v"
                                             (Just
                                               (ExprLiteral
                                                (LInitList [ ExprLiteral (LInt 5)
                                                           , ExprLiteral (LInt 6)
                                                           ]
                                                )))
                                            )
                                          ]

  describe "multiple" $ do
    pass "int x, y"         [ DeclVar (        QType (Type "int") ) "x" Nothing
                            , DeclVar (        QType (Type "int") ) "y" Nothing ]
    pass "const int x, y"   [ DeclVar (QConst (QType (Type "int"))) "x" Nothing
                            , DeclVar (QConst (QType (Type "int"))) "y" Nothing ]
    pass "int* x, y"        [ DeclVar (Qptr   (QType (Type "int"))) "x" Nothing
                            , DeclVar (       (QType (Type "int"))) "y" Nothing ]
    pass "int *x, y"        [ DeclVar (Qptr   (QType (Type "int"))) "x" Nothing
                            , DeclVar (       (QType (Type "int"))) "y" Nothing ]
    pass "int x, *y"        [ DeclVar (       (QType (Type "int"))) "x" Nothing
                            , DeclVar (Qptr   (QType (Type "int"))) "y" Nothing ]
    pass "int *x, &y"       [ DeclVar (Qptr   (QType (Type "int"))) "x" Nothing
                            , DeclVar (Qref   (QType (Type "int"))) "y" Nothing ]

    pass "int x : 1, y : 2" [ DeclVar (Qbitfield
                                       (ExprLiteral (LInt 1))
                                        (QType (Type "int"))
                                      ) "x" Nothing
                            , DeclVar (Qbitfield
                                       (ExprLiteral (LInt 2))
                                        (QType (Type "int"))
                                      ) "y" Nothing
                            ]

    -- Yes, the following is valid c++, grim, but valid
    -- (at least inside a struct, since bitfields only work in structs)
    pass "int& ref, *const ptr = nullptr, **ptr2, fix[5], bit : 3, flag:1, flex[]"
      [ DeclVar (Qref       (QType (Type "int" ))) "ref"  Nothing
      , DeclVar (Qconstptr  (QType (Type "int" ))) "ptr"  (Just (ExprLiteral LNullptr))
      , DeclVar (Qptr (Qptr (QType (Type "int")))) "ptr2" Nothing
      , DeclVar (Qarray     (ExprLiteral (LInt 5)) (QType (Type "int"))) "fix"  Nothing
      , DeclVar (Qbitfield  (ExprLiteral (LInt 3)) (QType (Type "int"))) "bit"  Nothing
      , DeclVar (Qbitfield  (ExprLiteral (LInt 1)) (QType (Type "int"))) "flag" Nothing
      , DeclVar (Qflexarray (QType (Type "int" ))) "flex" Nothing
      ]

  describe "realworld" $ do
    pass "xen::Color* pixel_data = (xen::Color*)stbi_load(file_path, &width, &height, &components, 4)"
      [DeclVar
       (Qptr (QType (Tmem (Type "xen") (Type "Color"))))
       "pixel_data"
       (Just
        (ExprPrefix
         (CCast (Qptr (QType (Tmem (Type "xen") (Type "Color")))))
         (ExprPostfix
          (ExprIdentifier "stbi_load")
          (Call [ ExprIdentifier "file_path"
                , ExprPrefix AddressOf (ExprIdentifier "width")
                , ExprPrefix AddressOf (ExprIdentifier "height")
                , ExprPrefix AddressOf (ExprIdentifier "components")
                , ExprLiteral (LInt 4)
                ]
          )))
       )
      ]
  where
    pass input output = itShouldParse (declVariable <* eof) input output
    fail input        = itShouldFail  (declVariable <* eof) input

--------------------------------------------------------------------------------

suite_declunion = describe "declUnion" $ do
  pass "union X { }" (DeclUnion "X" [])
  where
    pass input output = itShouldParse (declUnion <* eof) input output
    fail input        = itShouldFail  (declUnion <* eof) input

suite_decltype = describe "declType" $ do
  pass "struct A             { }" (DeclType "A" [] [])
  pass "struct B :         A { }" (DeclType "B" [(Public,  Type "A")] [])
  pass "struct B : private A { }" (DeclType "B" [(Private, Type "A")] [])
  pass "struct B : public  A { }" (DeclType "B" [(Public,  Type "A")] [])
  pass "class  B :         A { }" (DeclType "B" [(Private, Type "A")] [])
  pass "class  B : private A { }" (DeclType "B" [(Private, Type "A")] [])
  pass "class  B : public  A { }" (DeclType "B" [(Public,  Type "A")] [])
  pass "struct A : X, Y      { }" (DeclType "A" [(Public,  Type "X"), (Public, Type "Y")] [])

  pass "struct A {          int x; }" (DeclType "A" []
                                        [ (Public, DeclVar (QType (Type "int")) "x" Nothing) ]
                                      )
  pass "struct A { private: int x; }" (DeclType "A" []
                                       [ (Private, DeclVar (QType (Type "int")) "x" Nothing) ]
                                      )
  pass "class  A {         int x; }"  (DeclType "A" []
                                         [ (Private, DeclVar (QType (Type "int")) "x" Nothing) ]
                                      )
  pass "class  A { public: int x; }"   (DeclType "A" []
                                         [ (Public, DeclVar (QType (Type "int")) "x" Nothing) ]
                                      )

  pass "struct A { int x, y; }" (DeclType "A" []
                                 [ (Public, DeclVar (QType (Type "int")) "x" Nothing)
                                 , (Public, DeclVar (QType (Type "int")) "y" Nothing)
                                 ]
                                )
  pass "struct A { int x, y; long z; }" (
    DeclType "A" []
      [ (Public, DeclVar (QType (Type "int" )) "x" Nothing)
      , (Public, DeclVar (QType (Type "int" )) "y" Nothing)
      , (Public, DeclVar (QType (Type "long")) "z" Nothing)
      ]
    )


  pass "struct A { int x; private: int y; }" (
    DeclType "A" []
      [ (Public,  DeclVar (QType (Type "int" )) "x" Nothing)
      , (Private, DeclVar (QType (Type "int" )) "y" Nothing)
      ]
    )

  where
    pass input output = itShouldParse (declType <* eof) input output
    fail input        = itShouldFail  (declType <* eof) input

--------------------------------------------------------------------------------

suite_typeid = describe "typeid" $ do
  pass "int"                 (Type "int")
  pass "float"               (Type "float")
  pass "unsigned int"        (Type "unsigned int")
  pass "signed"              (Type "signed")
  pass "unsigned long long"  (Type "unsigned long long")
  pass "unsignedint"         (Type "unsignedint") -- technically a valid typename

  pass "xen::Window"         (Tmem (Type "xen") (Type "Window"))
  fail "xen.Window"
  fail "xen->Window"

  pass "Vec3<float>"     (Tinst (Type "Vec3") [TParamType (QType (Type "float"))])
  pass "Vec<3, float>"   (Tinst (Type "Vec") [TParamExpr (ExprLiteral (LInt 3))
                                             , TParamType (QType (Type "float"))
                                             ]
                         )

  pass "T<U<X>>"         (Tinst
                          (Type "T")
                          [ TParamType
                            (QType (Tinst
                                    (Type "U")
                                    [TParamType (QType (Type "X"))]
                                   )
                            )
                          ]
                         )

  pass "T<U::A<X>>"    (Tinst
                         (Type "T")
                         [ TParamType
                           (QType (Tmem
                                    (Type "U")
                                    (Tinst
                                      (Type "A")
                                      [TParamType (QType (Type "X"))]
                                    )
                                  )
                           )
                         ]
                       )

  pass "Vec<x+y, float>" (Tinst (Type "Vec") [TParamExpr (ExprBinary
                                                          (ExprIdentifier "x")
                                                           OpAdd
                                                           (ExprIdentifier "y")
                                                         )
                                             , TParamType (QType (Type "float"))
                                             ]
                         )

  -- Broken since parser things closing > is part of expression
  -- GCC can handle:
  -- template <bool T>
  -- struct Test { int elems[T]; };
  -- Test<(A > B)> test;
  -- But it does fail without the () around A > B
  --
  --pass "Vec<float, x+y>" (Tinst (Type "Vec") [ TParamType (QType (Type "float"))
  --                                           , TParamExpr (ExprBinary
  --                                                         (ExprIdentifier "x")
  --                                                          OpAdd
  --                                                          (ExprIdentifier "y")
  --                                                        )
  --                                           ]
  --                       )
  where
    pass input output = itShouldParse (typeid <* eof) input output
    fail input        = itShouldFail  (typeid <* eof) input

suite_qtype = describe "qtype" $ do
  pass "int"                (        (           (QType (Type "int"))))
  pass "const A"            (        (QConst     (QType (Type "A"  ))))
  pass "static A"           (        (QStatic    (QType (Type "A"  ))))
  pass "const static A"     (QConst  (QStatic    (QType (Type "A"  ))))
  pass "static constexpr A" (QStatic (QConstexpr (QType (Type "A"  ))))

  pass "A[3]" (Qarray     (ExprLiteral (LInt 3)) (QType (Type "A")))
  pass "A:3"  (Qbitfield  (ExprLiteral (LInt 3)) (QType (Type "A")))
  pass "A[]"  (Qflexarray (QType (Type "A")))

  pass "A[3][x]" (Qarray
                   (ExprLiteral (LInt 3))
                   (Qarray (ExprIdentifier "x") (QType (Type "A")))
                 )

  pass "int*"       (Qptr      (QType (Type "int" )))
  pass "int* const" (Qconstptr (QType (Type "int" )))
  pass "int&"       (Qref      (QType (Type "int" )))
  pass "void*"      (Qptr      (QType (Type "void")))

  pass "const unsigned int** const[5]" (
    Qarray
      (ExprLiteral (LInt 5))
      (Qconstptr (Qptr (QConst (QType (Type "unsigned int")))))
    )

  pass "static xen::Array<const Type<U>::Name*>&"
    (Qref
      (QStatic
        (QType
         (Tmem
          (Type "xen")
          (Tinst
            (Type "Array")
            [TParamType
              (Qptr (
                  QConst (
                      QType (Tmem (Tinst (Type "Type") [TParamType (QType (Type "U"))]) (Type "Name")))
                  )
              )
            ]
          )
         )
        )
      )
    )

  where
    pass input output = itShouldParse (qtype <* eof) input output
    fail input        = itShouldFail  (qtype <* eof) input
