#include <xen/math/Vector.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Vec3r", "[math][Vector]"){

	SECTION("Comparison Operators"){
		REQUIRE(operator==(Vec3r{1,1,1}, Vec3r{1,1,1}) == true );
		REQUIRE(operator!=(Vec3r{1,1,1}, Vec3r{1,1,1}) == false);

		REQUIRE(operator==(Vec3r{1,1, 1.1}, Vec3r{1,1, 0.9 + 0.2}) == true );
		REQUIRE(operator!=(Vec3r{1,1, 1.1}, Vec3r{1,1, 0.9 + 0.2}) == false);

		REQUIRE(operator==(Vec3r{1,1,1.001}, Vec3r{1,1,1}) == false);
		REQUIRE(operator!=(Vec3r{1,1,1.001}, Vec3r{1,1,1}) == true );
	}

	SECTION("Length/Magnitude/Distance/Squared"){
		REQUIRE(xen::length  (Vec3r{5,0,0}) ==  5);
		REQUIRE(xen::mag     (Vec3r{5,0,0}) ==  5);
		REQUIRE(xen::lengthSq(Vec3r{5,0,0}) == 25);
		REQUIRE(xen::magSq   (Vec3r{5,0,0}) == 25);

		REQUIRE(xen::length  (Vec3r{0,3,0}) == 3);
		REQUIRE(xen::mag     (Vec3r{0,3,0}) == 3);
		REQUIRE(xen::lengthSq(Vec3r{0,3,0}) == 9);
		REQUIRE(xen::magSq   (Vec3r{0,3,0}) == 9);

		REQUIRE(xen::length  (Vec3r{0,0,4}) ==  4);
		REQUIRE(xen::mag     (Vec3r{0,0,4}) ==  4);
		REQUIRE(xen::lengthSq(Vec3r{0,0,4}) == 16);
		REQUIRE(xen::magSq   (Vec3r{0,0,4}) == 16);

		REQUIRE(xen::length  (Vec3r{3,0,4}) ==  5);
		REQUIRE(xen::mag     (Vec3r{3,0,4}) ==  5);
		REQUIRE(xen::lengthSq(Vec3r{3,0,4}) == 25);
		REQUIRE(xen::magSq   (Vec3r{3,0,4}) == 25);

		REQUIRE(xen::distance  (Vec3r{1,0,0}, Vec3r{-5, 0, 0}) ==  6);
		REQUIRE(xen::distanceSq(Vec3r{1,0,0}, Vec3r{-5, 0, 0}) == 36);

		REQUIRE(xen::distance  (Vec3r{1,-2,0}, Vec3r{5,-5,0}) ==  5);
		REQUIRE(xen::distanceSq(Vec3r{1,-2,0}, Vec3r{5,-5,0}) == 25);
	}

	SECTION("Angle Between"){

		SECTION("Normalized"){
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 1, 0, 0})) ==   0_deg);
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{-1, 0, 0})) == 180_deg);
		}

		SECTION("Non-Normalized"){
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 5, 0, 0})) ==   0_deg);
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{-5, 0, 0})) == 180_deg);
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 0, 1, 0})) ==  90_deg);
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 0, 0, 1})) ==  90_deg);
			REQUIRE((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 1, 0, 1})) ==  45_deg);
		}
	}

}
