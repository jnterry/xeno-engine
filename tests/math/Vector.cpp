#include <xen/math/Vector.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Vec3r", "[math][Vector]"){

	SECTION("Comparison Operators"){
		CHECK(operator==(Vec3r{1,1,1}, Vec3r{1,1,1}) == true );
		CHECK(operator!=(Vec3r{1,1,1}, Vec3r{1,1,1}) == false);

		CHECK(operator==(Vec3r{1,1, 1.1}, Vec3r{1,1, 0.9 + 0.2}) == true );
		CHECK(operator!=(Vec3r{1,1, 1.1}, Vec3r{1,1, 0.9 + 0.2}) == false);

		CHECK(operator==(Vec3r{1,1,1.001}, Vec3r{1,1,1}) == false);
		CHECK(operator!=(Vec3r{1,1,1.001}, Vec3r{1,1,1}) == true );
	}

	SECTION("Length/Magnitude/Distance/Squared"){
		CHECK(xen::length  (Vec3r{5,0,0}) ==  5);
		CHECK(xen::mag     (Vec3r{5,0,0}) ==  5);
		CHECK(xen::lengthSq(Vec3r{5,0,0}) == 25);
		CHECK(xen::magSq   (Vec3r{5,0,0}) == 25);

		CHECK(xen::length  (Vec3r{0,3,0}) == 3);
		CHECK(xen::mag     (Vec3r{0,3,0}) == 3);
		CHECK(xen::lengthSq(Vec3r{0,3,0}) == 9);
		CHECK(xen::magSq   (Vec3r{0,3,0}) == 9);

		CHECK(xen::length  (Vec3r{0,0,4}) ==  4);
		CHECK(xen::mag     (Vec3r{0,0,4}) ==  4);
		CHECK(xen::lengthSq(Vec3r{0,0,4}) == 16);
		CHECK(xen::magSq   (Vec3r{0,0,4}) == 16);

		CHECK(xen::length  (Vec3r{3,0,4}) ==  5);
		CHECK(xen::mag     (Vec3r{3,0,4}) ==  5);
		CHECK(xen::lengthSq(Vec3r{3,0,4}) == 25);
		CHECK(xen::magSq   (Vec3r{3,0,4}) == 25);

		CHECK(xen::distance  (Vec3r{1,0,0}, Vec3r{-5, 0, 0}) ==  6);
		CHECK(xen::distanceSq(Vec3r{1,0,0}, Vec3r{-5, 0, 0}) == 36);

		CHECK(xen::distance  (Vec3r{1,-2,0}, Vec3r{5,-5,0}) ==  5);
		CHECK(xen::distanceSq(Vec3r{1,-2,0}, Vec3r{5,-5,0}) == 25);
	}

	SECTION("Angle Between"){

		SECTION("Normalized"){
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 1, 0, 0})) ==   0_deg);
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{-1, 0, 0})) == 180_deg);
		}

		SECTION("Non-Normalized"){
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 5, 0, 0})) ==   0_deg);
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{-5, 0, 0})) == 180_deg);
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 0, 1, 0})) ==  90_deg);
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 0, 0, 1})) ==  90_deg);
			CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 1, 0, 1})) ==  45_deg);
		}
	}

}
