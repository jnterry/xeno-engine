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

TEST_CASE("endsWith", "[core][string]"){
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

TEST_CASE("startsWith", "[core][string]"){
	CHECK(xen::startsWith("",       ""     ) == true);
	CHECK(xen::startsWith("abc",    ""     ) == true);
	CHECK(xen::startsWith("abc",    "a"    ) == true);
	CHECK(xen::startsWith("abc",    "ab"   ) == true);
	CHECK(xen::startsWith("abc",    "abc"  ) == true);
	CHECK(xen::startsWith("abc",    "abcd" ) == false);
	CHECK(xen::startsWith("abc",    "aba"  ) == false);
	CHECK(xen::startsWith("ababaa", "abaa" ) == false);
}
