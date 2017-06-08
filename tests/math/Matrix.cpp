#include <xen/math/Matrix.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>



TEST_CASE("Mat4r - Translation Generation", "[Matrix][math]"){
	#define TRANSLATION_TEST(a, b) CHECK(((a) * xen::Translation3d(b)) == ((a) + (b)))
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

TEST_CASE("Mat4r - Scale Generation", "[Matrix][math]"){
	#define SCALE_TEST(a, b) CHECK(((a) * xen::Scale3d(b)) == ((a) * (b)))
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
	#undef SCALE_TEST
}

TEST_CASE("Mat4r Determinant", "[math][Matrix]"){
	// results calculated with: http://matrix.reshish.com/detCalculation.php
	CHECK((xen::determinant(Mat4r{ 1,2,3,4,  -1,-2,-3,-4,  9,8,7,6, -6,-7,-8,-9 })) ==      0);
	CHECK((xen::determinant(Mat4r{ 2,2,4,2,  5,1,3,3,  4,0,5,7,  3,8,4,5        })) == -  414);
	CHECK((xen::determinant(Mat4r{ 6,2,4,9,  5,8,2,6,  10,3,11,34,  72,1,0,5    })) ==  20425);
	CHECK((xen::determinant(Mat4r{ 9,11,7,1,  4,2,13,14,  5,6,3,15,  10,8,12,0  })) == - 4394);
}

// :TODO: add more here, and Vec3r tests
TEST_CASE("Vec4r * Mat4r", "[math][Matrix]"){
	// calced using http://matrix.reshish.com/multCalculation.php
	CHECK((Vec4r(1_r,2_r,-3_r,-4_r) * Mat4r{1,13,7,4,  8,2,11,12,  5,15,9,6,  14,3,10,16}) ==
	      Vec4r(-54_r,-40_r,-38_r,-54_r));
}
