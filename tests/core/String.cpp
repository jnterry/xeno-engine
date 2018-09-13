////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen string type and helper functions
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/String.hpp>
#include <catch.hpp>

TEST_CASE("stringLength", "[core][string]"){
	CHECK(xen::stringLength(""                      ) == 0);
	CHECK(xen::stringLength(xen::makeString("")     ) == 0);
	CHECK(xen::stringLength(xen::String::Empty      ) == 0);

	CHECK(xen::stringLength(" a "                    ) == 3);
	CHECK(xen::stringLength(xen::makeString(" b ")   ) == 3);

	CHECK(xen::stringLength("hello"                 ) == 5);
	CHECK(xen::stringLength(xen::makeString("hello")) == 5);
}

TEST_CASE("endsWith(const char*, const char*)", "[core][string]"){
	CHECK(xen::endsWith("",        ""    ) == true);
	CHECK(xen::endsWith("hello",   ""    ) == true);
	CHECK(xen::endsWith("abc",     "abc" ) == true);
	CHECK(xen::endsWith("abc",     "bc"  ) == true);
	CHECK(xen::endsWith("abc",     "c"   ) == true);
	CHECK(xen::endsWith("abc",     ""    ) == true);
	CHECK(xen::endsWith("abc",     "1abc") == false);
	CHECK(xen::endsWith("abc1",    "abc" ) == false);
	CHECK(xen::endsWith("ababab",  "ab"  ) == true);
	CHECK(xen::endsWith("abababa", "ab"  ) == false);
	CHECK(xen::endsWith("abababa", "aba" ) == true);
}

TEST_CASE("endsWith(xen::String, const char*)", "[core][string]"){
	CHECK(xen::endsWith(xen::makeString(""       ), ""    ) == true);
	CHECK(xen::endsWith(xen::makeString("hello"  ), ""    ) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), "abc" ) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), "bc"  ) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), "c"   ) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), ""    ) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), "1abc") == false);
	CHECK(xen::endsWith(xen::makeString("abc1"   ), "abc" ) == false);
	CHECK(xen::endsWith(xen::makeString("ababab" ), "ab"  ) == true);
	CHECK(xen::endsWith(xen::makeString("abababa"), "ab"  ) == false);
	CHECK(xen::endsWith(xen::makeString("abababa"), "aba" ) == true);
}

TEST_CASE("endsWith(xen::String, xen::String)", "[core][string]"){
	CHECK(xen::endsWith(xen::makeString(""       ), xen::makeString(""    )) == true);
	CHECK(xen::endsWith(xen::makeString("hello"  ), xen::makeString(""    )) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), xen::makeString("abc" )) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), xen::makeString("bc"  )) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), xen::makeString("c"   )) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), xen::makeString(""    )) == true);
	CHECK(xen::endsWith(xen::makeString("abc"    ), xen::makeString("1abc")) == false);
	CHECK(xen::endsWith(xen::makeString("abc1"   ), xen::makeString("abc" )) == false);
	CHECK(xen::endsWith(xen::makeString("ababab" ), xen::makeString("ab"  )) == true);
	CHECK(xen::endsWith(xen::makeString("abababa"), xen::makeString("ab"  )) == false);
	CHECK(xen::endsWith(xen::makeString("abababa"), xen::makeString("aba" )) == true);
}

TEST_CASE("endsWith char", "[core][string]"){
	CHECK(xen::endsWith(xen::makeString("a"      ), 'a' ) == true );
	CHECK(xen::endsWith(xen::makeString(""       ), 'a' ) == false);
	CHECK(xen::endsWith(xen::makeString("ab"     ), 'a' ) == false);
	CHECK(xen::endsWith(xen::makeString("abababa"), 'a' ) == true );
	CHECK(xen::endsWith(xen::makeString("abababa"), '\0') == false);
	CHECK(xen::endsWith(xen::makeString(""       ), '\0') == false);

	CHECK(xen::endsWith(               ("a"      ), 'a' ) == true );
	CHECK(xen::endsWith(               (""       ), 'a' ) == false);
	CHECK(xen::endsWith(               ("ab"     ), 'a' ) == false);
	CHECK(xen::endsWith(               ("abababa"), 'a' ) == true );
	CHECK(xen::endsWith(               ("abababa"), '\0') == false);
	CHECK(xen::endsWith(               (""       ), '\0') == false);
}

TEST_CASE("startsWith(const char*, const char*)", "[core][string]"){
	CHECK(xen::startsWith("",       ""     ) == true);
	CHECK(xen::startsWith("abc",    ""     ) == true);
	CHECK(xen::startsWith("abc",    "a"    ) == true);
	CHECK(xen::startsWith("abc",    "ab"   ) == true);
	CHECK(xen::startsWith("abc",    "abc"  ) == true);
	CHECK(xen::startsWith("abc",    "abcd" ) == false);
	CHECK(xen::startsWith("abc",    "aba"  ) == false);
	CHECK(xen::startsWith("ababaa", "abaa" ) == false);
}

TEST_CASE("startsWith(xen::String, const char*)", "[core][string]"){
	CHECK(xen::startsWith(xen::makeString(""      ), ""     ) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), ""     ) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), "a"    ) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), "ab"   ) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), "abc"  ) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), "abcd" ) == false);
	CHECK(xen::startsWith(xen::makeString("abc"   ), "aba"  ) == false);
	CHECK(xen::startsWith(xen::makeString("ababaa"), "abaa" ) == false);
}

TEST_CASE("startsWith(xen::String, xen::String)", "[core][string]"){
	CHECK(xen::startsWith(xen::makeString(""      ), xen::makeString(""     )) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), xen::makeString(""     )) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), xen::makeString("a"    )) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), xen::makeString("ab"   )) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), xen::makeString("abc"  )) == true);
	CHECK(xen::startsWith(xen::makeString("abc"   ), xen::makeString("abcd" )) == false);
	CHECK(xen::startsWith(xen::makeString("abc"   ), xen::makeString("aba"  )) == false);
	CHECK(xen::startsWith(xen::makeString("ababaa"), xen::makeString("abaa" )) == false);
}


TEST_CASE("startsWith char", "[core][string]"){
	CHECK(xen::startsWith(xen::makeString("a"      ), 'a' ) == true );
	CHECK(xen::startsWith(xen::makeString(""       ), 'a' ) == false);
	CHECK(xen::startsWith(xen::makeString("ab"     ), 'a' ) == true );
	CHECK(xen::startsWith(xen::makeString("bababa" ), 'a' ) == false);
	CHECK(xen::startsWith(xen::makeString("abababa"), '\0') == false);
	CHECK(xen::startsWith(xen::makeString(""       ), '\0') == true );

	CHECK(xen::startsWith(               ("a"      ), 'a' ) == true );
	CHECK(xen::startsWith(               (""       ), 'a' ) == false);
	CHECK(xen::startsWith(               ("ab"     ), 'a' ) == true );
	CHECK(xen::startsWith(               ("bababa" ), 'a' ) == false);
	CHECK(xen::startsWith(               ("abababa"), '\0') == false);
	CHECK(xen::startsWith(               (""       ), '\0') == true );
}
