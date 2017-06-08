#include <xen/math/Matrix.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>



TEST_CASE("Mat4r - Transform Generations", "[Matrix][math]"){

	SECTION("Translation"){
		#define TRANSLATION_TEST(a, b) REQUIRE(((a) * xen::Translation3d(b)) == ((a) + (b)))
		TRANSLATION_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 0, 0, 0}));
		TRANSLATION_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 1, 0, 0}));
		TRANSLATION_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 0, 1, 0}));
		TRANSLATION_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 0, 0, 1}));
		TRANSLATION_TEST((Vec3r{ 0, 0, 0}), (Vec3r{ 1, 2, 3}));
		TRANSLATION_TEST((Vec3r{ 5,10,-5}), (Vec3r{-6, 1, 6}));
		for(float x = -10; x <= 10; x += 5){
			for(float y = -10; y <= 10; y += 5){
				for(float z = -10; z <= 10; z += 5){
					TRANSLATION_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ x, y, z}));
				}
			}
		}
		#undef TRANSLATION_TEST
	}

	SECTION("Rotation3dx"){
		REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3dx(  90_deg)) == ( Vec3r{0,  0,  0}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dx(  90_deg)) == ( Vec3r{0,  0, -1}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dx(- 90_deg)) == ( Vec3r{0,  0,  1}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dx( 270_deg)) == ( Vec3r{0,  0,  1}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dx( 180_deg)) == ( Vec3r{0, -1,  0}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dx( 360_deg)) == ( Vec3r{0,  1,  0}));
		REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3dx(  45_deg)) == ( Vec3r{1,  0,  0}));
		REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3dx(  90_deg)) == ( Vec3r{1,  0,  0}));
		REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3dx( 180_deg)) == ( Vec3r{1,  0,  0}));
		REQUIRE(((Vec3r{1, -1,  0}) * xen::Rotation3dx( 180_deg)) == ( Vec3r{1,  1,  0}));
	}

	SECTION("Rotation3dy"){
		REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3dy(  90_deg))  == ( Vec3r{0,  0,  0}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dy(  45_deg))  == ( Vec3r{0,  1,  0}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dy(  90_deg))  == ( Vec3r{0,  1,  0}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dy( 120_deg))  == ( Vec3r{0,  1,  0}));
		REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3dy(- 90_deg))  == ( Vec3r{0,  0, -1}));
		REQUIRE(((Vec3r{6,  0,  0}) * xen::Rotation3dy(- 90_deg))  == ( Vec3r{0,  0, -6}));
		REQUIRE(((Vec3r{1,  5,  0}) * xen::Rotation3dy( 270_deg))  == ( Vec3r{0,  5, -1}));
		REQUIRE(((Vec3r{0,  0,  1}) * xen::Rotation3dy( 180_deg))  == ( Vec3r{0,  0, -1}));
	}

	SECTION("Rotation3dz"){
		REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3dz(  90_deg))  == ( Vec3r{0,  0,  0}));
		REQUIRE(((Vec3r{0,  0,  3}) * xen::Rotation3dz(  45_deg))  == ( Vec3r{0,  0,  3}));
		REQUIRE(((Vec3r{0,  0, -4}) * xen::Rotation3dz(  90_deg))  == ( Vec3r{0,  0, -4}));
		REQUIRE(((Vec3r{0,  0, 10}) * xen::Rotation3dz( 220_deg))  == ( Vec3r{0,  0, 10}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dz(  90_deg))  == ( Vec3r{1,  0,  0}));
		REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3dz(- 90_deg))  == ( Vec3r{0,  1,  0}));
		REQUIRE(((Vec3r{3,  0,  0}) * xen::Rotation3dz(- 90_deg))  == ( Vec3r{0,  3,  0}));
		REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3dz( 270_deg))  == ( Vec3r{0,  1,  0}));
		REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3dz( 180_deg))  == ( Vec3r{0, -1,  0}));
	}

	SECTION("Scale3d"){
		#define SCALE_TEST(a, b) REQUIRE(((a) * xen::Scale3d(b)) == ((a) * (b)))
		SCALE_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 0, 0, 0}));
		SCALE_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 1, 0, 0}));
		SCALE_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 0, 1, 0}));
		SCALE_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ 0, 0, 1}));
		SCALE_TEST((Vec3r{ 0, 0, 0}), (Vec3r{ 1, 2, 3}));
		SCALE_TEST((Vec3r{ 5,10,-5}), (Vec3r{-6, 1, 6}));
		for(float x = -10; x <= 10; x += 5){
			for(float y = -10; y <= 10; y += 5){
				for(float z = -10; z <= 10; z += 5){
					SCALE_TEST((Vec3r{ 1, 2, 3}), (Vec3r{ x, y, z}));
				}
			}
		}
		#undef TRANSLATION_TEST
	}
}

TEST_CASE("Mat4r Determinant", "[math][Matrix]"){
	// results calculated with: http://matrix.reshish.com/detCalculation.php
	REQUIRE((xen::determinant(Mat4r{ 1,2,3,4,  -1,-2,-3,-4,  9,8,7,6, -6,-7,-8,-9 })) ==      0);
	REQUIRE((xen::determinant(Mat4r{ 2,2,4,2,  5,1,3,3,  4,0,5,7,  3,8,4,5        })) == -  414);
	REQUIRE((xen::determinant(Mat4r{ 6,2,4,9,  5,8,2,6,  10,3,11,34,  72,1,0,5    })) ==  20425);
	REQUIRE((xen::determinant(Mat4r{ 9,11,7,1,  4,2,13,14,  5,6,3,15,  10,8,12,0  })) == - 4394);
}
