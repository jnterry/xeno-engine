#include <xen/math/vector.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Vec3r, Comparison Operators", "[math][Vector]"){
	CHECK(operator==(Vec3r{1,1,1}, Vec3r{1,1,1}) == true );
	CHECK(operator!=(Vec3r{1,1,1}, Vec3r{1,1,1}) == false);

	CHECK(operator==(Vec3r{1,1, 1.1}, Vec3r{1,1, 0.9 + 0.2}) == true );
	CHECK(operator!=(Vec3r{1,1, 1.1}, Vec3r{1,1, 0.9 + 0.2}) == false);

	CHECK(operator==(Vec3r{1,1,1.001}, Vec3r{1,1,1}) == false);
	CHECK(operator!=(Vec3r{1,1,1.001}, Vec3r{1,1,1}) == true );
}

TEST_CASE("Vec3r Length/Magnitude/Distance/Squared", "[math][Vector]"){
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

TEST_CASE("Vec3r Angle Between", "[math][Vector]"){
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

TEST_CASE("Vec3r Dot Product", "[math][Vector]"){

	SECTION("Orthoganal Vectors have dot of 0"){
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r(0,1,0)) == Approx(0));
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r(0,0,1)) == Approx(0));
		CHECK(xen::dot(Vec3r(0,1,0), Vec3r(0,0,1)) == Approx(0));

		CHECK(xen::dot(Vec3r(3,0,0), Vec3r(0,3,0)) == Approx(0));
		CHECK(xen::dot(Vec3r(6,0,0), Vec3r(0,0,6)) == Approx(0));
		CHECK(xen::dot(Vec3r(0,8,0), Vec3r(0,0,8)) == Approx(0));

		CHECK(xen::dot(Vec3r(3,0,0), Vec3r(0,5,0)) == Approx(0));
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r(0,0,4)) == Approx(0));
		CHECK(xen::dot(Vec3r(0,2,0), Vec3r(0,0,7)) == Approx(0));
	}

	SECTION("Opposite Vectors have dot of -1"){
		CHECK(xen::dot(Vec3r(0,1,0), Vec3r( 0,-1, 0)) == Approx(-1));
		CHECK(xen::dot(xen::normalized(Vec3r(3,0,0)), xen::normalized(Vec3r(-3, 0, 0))) == Approx(-1));
		CHECK(xen::dot(xen::normalized(Vec3r(0,0,7)), xen::normalized(Vec3r( 0, 0,-7))) == Approx(-1));
	}

	SECTION("Equal Vectors have dot of 1"){
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r( 1, 0, 0)) == Approx( 1));
		CHECK(xen::dot(xen::normalized(Vec3r(3,0,0)), xen::normalized(Vec3r( 3, 0, 0))) == Approx( 1));
		CHECK(xen::dot(xen::normalized(Vec3r(3,2,0)), xen::normalized(Vec3r( 3, 2, 0))) == Approx( 1));
	}


	SECTION("Arbitary Vectors"){
	        CHECK(xen::dot(Vec3r(1,2,3)  , Vec3r(5,9,-2)) == Approx(17));
	        CHECK(xen::dot(Vec3r(-10,6,8), Vec3r(2,-3,9)) == Approx(34));
	}
}


TEST_CASE("Vec3r Project onto Plane",  "[math][Vector]"){
	SECTION("Onto XY"){
		for(real x = -5; x <= 5; ++x){
			for(real y = -5; y <= 5; ++y){
				for(real z = -5; z <= 5; ++z){
					CHECK(xen::projectOntoPlane<real>({x,y,z},  Vec3r::UnitZ) == Vec3r(x,y,0));
					CHECK(xen::projectOntoPlane<real>({x,y,z}, -Vec3r::UnitZ) == Vec3r(x,y,0));
				}
			}
		}
	}

	SECTION("Onto XZ"){
		for(real x = -5; x <= 5; ++x){
			for(real y = -5; y <= 5; ++y){
				for(real z = -5; z <= 5; ++z){
					CHECK(xen::projectOntoPlane<real>({x,y,z},  Vec3r::UnitY) == Vec3r(x,0,z));
					CHECK(xen::projectOntoPlane<real>({x,y,z}, -Vec3r::UnitY) == Vec3r(x,0,z));
				}
			}
		}
	}

	SECTION("Onto YZ"){
		for(real x = -5; x <= 5; ++x){
			for(real y = -5; y <= 5; ++y){
				for(real z = -5; z <= 5; ++z){
					CHECK(xen::projectOntoPlane<real>({x,y,z},  Vec3r::UnitX) == Vec3r(0,y,z));
					CHECK(xen::projectOntoPlane<real>({x,y,z}, -Vec3r::UnitX) == Vec3r(0,y,z));
				}
			}
		}
	}

	//:TODO: onto arbitary planes
}

TEST_CASE("toHomo", "[math][Vector]"){
	CHECK(xen::toHomo(Vec3r{1,2,3}   ) == Vec4r{1,2,3,1});
	CHECK(xen::toHomo(Vec3r{1,2,3}, 0) == Vec4r{1,2,3,0});

	CHECK(xen::toHomo(Vec2r{6,7}   ) == Vec3r{6,7,1});
	CHECK(xen::toHomo(Vec3r{8,9}, 0) == Vec3r{8,9,0});
}

TEST_CASE("fromHomo", "[math][Vector]"){
	CHECK(xen::fromHomo(Vec4r{2,4,6,1})  == Vec3r{2,4,6});
	CHECK(xen::fromHomo(Vec4r{2,4,6,2})  == Vec3r{1,2,3});

	CHECK(xen::fromHomo(Vec2r{4,8,1})  == Vec3r{2,4});
	CHECK(xen::fromHomo(Vec2r{4,8,2})  == Vec3r{2,4});
}
