////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::Matrix
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/matrix.hpp>
#include <xen/math/vector.hpp>
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

TEST_CASE("Vec4r * Mat4r", "[math][Matrix]"){
	// calced using http://matrix.reshish.com/multiplication.php
	CHECK((Vec4r(1_r,2_r,-3_r,-4_r) * Mat4r{1,13,7,4,  8,2,11,12,  5,15,9,6,  14,3,10,16}) ==
	      Vec4r(-54_r,-40_r,-38_r,-54_r));

	CHECK((Vec4r(-5,0,6,3) * Mat4r{-5,3,8,-2,  0,9,4,2,  4,-1,-10,20,  15,17,9,13}) ==
	      Vec4r(94,30,-73,169));
}

TEST_CASE("Vec3r * Mat4r", "[math][Matrix]"){
	// calced using http://matrix.reshish.com/multiplication.php
	CHECK((Vec3r(3,6,-2) * Mat4r{3,5,7,2,  -1,0,3,-7,  -2,-5,3,10,  -4,0,9,3}) == (Vec3r(3,25,42) / -53_r));
	CHECK((Vec3r(-3,9,0) * Mat4r{3,0,1,0,   5,0,0,-2,  -6,-3,-8,1,   0,5,3,0}) == (Vec3r(36,5, 0) / -18_r));
}

TEST_CASE("Matrix Multiplication Order", "[math][Matrix]"){
	// This does translation then scale
	Mat4r transform = Mat4r::Identity;
	transform *= xen::Translation3d(3, 0, 0);
	transform *= xen::Scale3d      (2, 1, 1);
	CHECK((Vec3r(1,0,0) * transform == Vec3r(8, 0, 0)));

	// This does scale then translation
  transform = Mat4r::Identity;
	transform *= xen::Scale3d      (2, 1, 1);
	transform *= xen::Translation3d(3, 0, 0);
	CHECK((Vec3r(1,0,0) * transform == Vec3r(5, 0, 0)));
}

TEST_CASE("Matrix4x4 Inverse", "[math][Matrix]"){
	CHECK_THAT(xen::getInverse(Mat4r::Identity),
	           IsMat(Mat4r::Identity)
	          );

	CHECK_THAT(xen::getInverse(xen::Scale3d(2, 4, 8)),
	           IsMat(xen::Scale3d(0.5, 0.25, 0.125))
	          );

	CHECK_THAT(xen::getInverse(xen::Translation3d(2, 4, 8)),
	           IsMat(xen::Translation3d(-2, -4, -8))
	          );

	CHECK_THAT(xen::getInverse(xen::Rotation3dx(45_deg)),
	           IsMat(xen::Rotation3dx(-45_deg))
	          );

	CHECK_THAT(xen::getInverse(xen::Translation3d(1,2,3) *
	                           xen::Scale3d      (10, 20, 30) *
	                           xen::Rotation3dz  (90_deg)),
	           IsMat(xen::Rotation3dz(-90_deg) *
	                 xen::Scale3d    (1/10_r, 1/20_r, 1/30_r) *
	                 xen::Translation3d(-1, -2, -3)
	                )
	          );
}

TEST_CASE("Matrix row-vector accessor", "[math][Matrix][Vector]"){
	SECTION("2x4 matrix"){
		xen::Matrix<2,4,real> mat2x4 = { 10, 20, 30, 40,
		                                 50, 60, 70, 80 };

		CHECK(mat2x4[0] == Vec4r(10, 20, 30, 40));
		CHECK(mat2x4[1] == Vec4r(50, 60, 70, 80));

		mat2x4[0][1] = 5;
		mat2x4[1][2] = 6;

		CHECK_THAT(mat2x4, IsMat(xen::Matrix<2,4,real>{ 10,  5, 30, 40,
						                                        50, 60,  6, 80 })
		);
	}

	SECTION("3x3 matrix"){
		Mat3r mat3x3 = { 10, 20, 30,
		                 40, 50, 60,
		                 70, 80, 90 };

		CHECK(mat3x3[0] == Vec3r(10, 20, 30));
		CHECK(mat3x3[1] == Vec3r(40, 50, 60));
		CHECK(mat3x3[2] == Vec3r(70, 80, 90));

		mat3x3[0][0] = 1;
		mat3x3[1][1] = 1;
		mat3x3[2][2] = 1;

		CHECK_THAT(mat3x3, IsMat(Mat3r{  1, 20, 30,
						                        40,  1, 60,
						                        70, 80,  1 })
		);
	}

	SECTION("4x4 matrix"){
		Mat4r mat4x4 = { 11, 12, 13, 14,
		                 21, 22, 23, 24,
		                 31, 32, 33, 34,
		                 41, 42, 43, 44 };

		CHECK(mat4x4[0] == Vec4r(11, 12, 13, 14));
		CHECK(mat4x4[1] == Vec4r(21, 22, 23, 24));
		CHECK(mat4x4[2] == Vec4r(31, 32, 33, 34));
		CHECK(mat4x4[3] == Vec4r(41, 42, 43, 44));

		mat4x4[0][3] = 1;
		mat4x4[1][2] = 1;
		mat4x4[2][1] = 1;
		mat4x4[3][0] = 1;

		CHECK_THAT(mat4x4, IsMat(Mat4r{ 11, 12, 13,  1,
		                                21, 22,  1, 24,
		                                31,  1, 33, 34,
						                         1, 42, 43, 44 })
		);
	}
}
