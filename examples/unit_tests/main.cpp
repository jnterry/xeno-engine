#define CATCH_CONFIG_MAIN
#include <catch.hpp>

TEST_CASE("test", "[test]"){
	REQUIRE(3 < 5);
	REQUIRE(2 == 2);
	REQUIRE(2 != 3);
	REQUIRE(1 > 5); //failure
}
