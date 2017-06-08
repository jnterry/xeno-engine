////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Rotation3d.hpp
/// \author Jamie Terry
/// \date 2017/06/08
/// \brief Contains unit tests for the various ways by which rotations can be
/// applied to a 3d point
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Quaternion.hpp>

#include "ostream_operators.hpp"

#include <catch.hpp>

// check matrix is valid rotation, IE, is of the form:
// a b c 0
// d e f 0
// g h i 0
// 0 0 0 1
// and has determinant of 1, hence:
//  - volume is preserved
//  - no reflection has occured
#define REQUIRE_IS_ROT_MAT(rot) \
	REQUIRE(xen::determinant(rot) == Approx(1.0_r)); \
	REQUIRE(rot.elements[ 3] == Approx(0)); \
	REQUIRE(rot.elements[ 7] == Approx(0)); \
	REQUIRE(rot.elements[11] == Approx(0)); \
	REQUIRE(rot.elements[12] == Approx(0)); \
	REQUIRE(rot.elements[13] == Approx(0)); \
	REQUIRE(rot.elements[14] == Approx(0)); \
	REQUIRE(rot.elements[15] == Approx(1));



TEST_CASE("Rotating point by 0 degrees has no effect",
          "[math][Matrix][Vector][Quaternion][AxisAngle]"){

	SECTION("Standard Axes"){
		CHECK_THAT(xen::Rotation3dx(0_deg), IsMat(Mat4r::Identity));
		CHECK_THAT(xen::Rotation3dy(0_deg), IsMat(Mat4r::Identity));
		CHECK_THAT(xen::Rotation3dz(0_deg), IsMat(Mat4r::Identity));
	}

	SECTION("Arbitary Axes"){
		for(real ax = -1; ax <= 1; ++ax){
			for(real ay = -1; ay <= 1; ++ay){
				for(real az = -1; az <= 1; ++az){
					if(ax == 0 && ay == 0 && az == 0){ continue; }
					Vec3r axis = xen::normalized(Vec3r(ax, ay, az));

					REQUIRE_THAT(xen::Rotation3d(                axis,     0_deg ),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::Quaternion(axis,     0_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::AxisAngle (axis,     0_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(                axis,   360_deg ),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::Quaternion(axis,   360_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::AxisAngle (axis,   360_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(                axis,  3600_deg ),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::Quaternion(axis,  3600_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::AxisAngle (axis,  3600_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(                axis, - 360_deg ),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::Quaternion(axis, - 360_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::AxisAngle (axis, - 360_deg)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(                axis,    15_rev ),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::Quaternion(axis,    15_rev)),
					             IsMat(Mat4r::Identity));
					REQUIRE_THAT(xen::Rotation3d(xen::AxisAngle (axis,    15_rev)),
					             IsMat(Mat4r::Identity));

					for(real px = -1; px <= 1; ++px){
						for(real py = -1; py <= 1; ++py){
							for(real pz = -1; pz <= 1; ++pz){
								Vec3r p(px, py, pz);
								REQUIRE(xen::rotated(p, axis, 0_deg) == p);
							}
						}
					}
				}
			}
		}
	}
}


TEST_CASE("Rotating point around axis upon which it lies has no effect",
          "[math][Matrix][Vector][Quaternion][AxisAngle]"){

	SECTION("Unit Axes"){
		for(int axis = 0; axis < 3; ++axis){
			for(real a_val = -100; a_val <= 100; a_val += 25){
				Vec3r p(0, 0, 0);
				p.elements[axis] = a_val;
				Mat4r rot;
				for(xen::Angle a = -300_deg; a <= 300_deg; a += 25_deg){
					SECTION("Single Euler Angle"){
						switch(axis){
						case 0: rot = xen::Rotation3dx(a); break;
						case 1: rot = xen::Rotation3dy(a); break;
						case 2: rot = xen::Rotation3dz(a); break;
						}
					    REQUIRE_IS_ROT_MAT(rot);
						CHECK((p * rot) == p);
					}

					SECTION("Rotated Function"){
						CHECK(xen::rotated(p, Vec3r::UnitAxes[axis], a) == p);
					}

					SECTION("From Axis Angle"){
						Mat4r rotA = xen::Rotation3d(Vec3r::UnitAxes[axis], a );
						Mat4r rotB = xen::Rotation3d(xen::AxisAngle(Vec3r::UnitAxes[axis], a));;
						REQUIRE(rotA == rotB);
						REQUIRE_IS_ROT_MAT(rotA);
						CHECK((p * rotA) == p);
					}

					SECTION("From Quaternion"){
						rot = xen::Rotation3d(Quat(Vec3r::UnitAxes[axis], a));
						REQUIRE_IS_ROT_MAT(rot);
						CHECK((p * rot) == p);
					}
				}
			}
		}
	}

	SECTION("Arbitary Axes"){
		for(real x = 0; x < 2; ++x){
			for(real y = 0; y < 2; ++y){
				for(real z = 0; z < 2; ++z){
					if(x == 0 && y == 0 && z == 0){ continue; }

					Vec3r axis = xen::normalized(Vec3r(x,y,z));
					REQUIRE(xen::mag(axis) == Approx(1.0_r));

					for(real factor = -10; factor <= 10; factor += 3){
						Vec3r p = factor * Vec3r(x,y,z);
						for(xen::Angle a = -300_deg; a <= 300_deg; a += 25_deg){
							CHECK(xen::rotated(p, axis, a) == p);

							Mat4r rot = xen::Rotation3d(axis, a);
							REQUIRE_IS_ROT_MAT(rot);
							CHECK((p * rot) == p);
						}
					}
				}
			}
		}
	}
}

/*
TEST_CASE("Rotating point by quaternion", "[math][Quaternion][AxisAngle]"){
	SECTION("Rot X"){
		REQUIRE((xen::rotated((Vec3r{0,  0,  0}), Quat(Vec3r::UnitX, 90_deg)))  ==
		        ( Vec3r{0,  0,  0}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitX, 90_deg)))  ==
		        ( Vec3r{0,  0, -1}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitX, -90_deg)))  ==
		        ( Vec3r{0,  0,  1}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitX, 270_deg)))  ==
		        ( Vec3r{0,  0,  1}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitX, 180_deg)))  ==
		        ( Vec3r{0, -1,  0}));
	}

	SECTION("Rot Y"){
		REQUIRE((xen::rotated((Vec3r{0,  0,  0}), Quat(Vec3r::UnitY,   90_deg))) ==
		        ( Vec3r{0,  0,  0}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitY,   45_deg))) ==
		        ( Vec3r{0,  1,  0}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitY,   90_deg))) ==
		        ( Vec3r{0,  1,  0}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitY,  120_deg))) ==
		        ( Vec3r{0,  1,  0}));
		REQUIRE((xen::rotated((Vec3r{1,  0,  0}), Quat(Vec3r::UnitY, - 90_deg))) ==
		        ( Vec3r{0,  0, -1}));
		REQUIRE((xen::rotated((Vec3r{5,  0,  0}), Quat(Vec3r::UnitY, - 90_deg))) ==
		        ( Vec3r{0,  0, -5}));
		REQUIRE((xen::rotated((Vec3r{1,  5,  0}), Quat(Vec3r::UnitY,  270_deg))) ==
		        ( Vec3r{0,  5, -1}));
		REQUIRE((xen::rotated((Vec3r{0,  0,  1}), Quat(Vec3r::UnitY,  180_deg))) ==
		        ( Vec3r{0,  0, -1}));
	}

	SECTION("Rot Z"){
		REQUIRE((xen::rotated((Vec3r{0,  0,  0}), Quat(Vec3r::UnitZ, 90_deg))  )==
		        ( Vec3r{0,  0,  0}));
		REQUIRE((xen::rotated((Vec3r{0,  0,  3}), Quat(Vec3r::UnitZ,  45_deg)) ) ==
		        ( Vec3r{0,  0,  3}));
		REQUIRE((xen::rotated((Vec3r{0,  0, -4}), Quat(Vec3r::UnitZ,  90_deg)) ) ==
		        ( Vec3r{0,  0, -4}));
		REQUIRE((xen::rotated((Vec3r{0,  0, 10}), Quat(Vec3r::UnitZ, 220_deg)) ) ==
		        ( Vec3r{0,  0, 10}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitZ, 90_deg))  )==
		        ( Vec3r{1,  0,  0}));
		REQUIRE((xen::rotated((Vec3r{1,  0,  0}), Quat(Vec3r::UnitZ, -90_deg)) ) ==
		        ( Vec3r{0,  1,  0}));
		REQUIRE((xen::rotated((Vec3r{3,  0,  0}), Quat(Vec3r::UnitZ, -90_deg)) ) ==
		        ( Vec3r{0,  3,  0}));
		REQUIRE((xen::rotated((Vec3r{1,  0,  0}), Quat(Vec3r::UnitZ, 270_deg)) ) ==
		        ( Vec3r{0,  1,  0}));
		REQUIRE((xen::rotated((Vec3r{0,  1,  0}), Quat(Vec3r::UnitZ, 180_deg)) ) ==
		        ( Vec3r{0, -1,  0}));
	}

}


TEST_CASE("Rotation Matrix from Quaterion (xen::Rotation3d())",
          "[math][Quaternion][AxisAngle]"){

	// correct rotations calculated with:
	// http://www.nh.cas.cz/people/lazar/celler/online_tools.php

	SECTION("Overloads Equivalent"){
		REQUIRE((xen::Rotation3d(               Vec3r{ 1,  2, 3},  25_deg)) ==
		        (xen::Rotation3d(xen::AxisAngle{Vec3r{ 1,  2, 3},  25_deg})));
		REQUIRE((xen::Rotation3d(               Vec3r{ 3,-10, 1}, -60_deg)) ==
		        (xen::Rotation3d(xen::AxisAngle{Vec3r{ 3,-10, 1}, -60_deg})));
	}

	SECTION("Rot X"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitX, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 90_deg))  ==
			        ( Vec3r{0,  0,  1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, -90_deg))  ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 270_deg))  ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 180_deg))  ==
			        ( Vec3r{0, -1,  0}));
			REQUIRE(((Vec3r{0,  3,  0}) * xen::Rotation3d(Vec3r::UnitX, 180_deg))  ==
			        ( Vec3r{0, -3,  0}));
			REQUIRE(((Vec3r{0,  0, -5}) * xen::Rotation3d(Vec3r::UnitX, 180_deg))  ==
			        ( Vec3r{0,  0,  5}));
			REQUIRE(((Vec3r{1,  2,  3}) * xen::Rotation3d(Vec3r::UnitX,  45_deg))  ==
			        ( Vec3r{1, -0.707107,  3.535534}));

			//THEN("Quaternion Matricies equivalent to Rotation Matricies"){
			//	REQUIRE((xen::Rotation3dx(  0_deg)) == (xen::Rotation3d(Vec3r::UnitX,   0_deg)));
			//	REQUIRE((xen::Rotation3dx( 90_deg)) == (xen::Rotation3d(Vec3r::UnitX,  90_deg)));
			//	REQUIRE((xen::Rotation3dx(180_deg)) == (xen::Rotation3d(Vec3r::UnitX, 180_deg)));
			//	REQUIRE((xen::Rotation3dx(270_deg)) == (xen::Rotation3d(Vec3r::UnitX, 270_deg)));
			//}
		}
	}

	SECTION("Rot Y"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitY,   90_deg)) ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitY,   45_deg)) ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitY,   90_deg)) ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitY,  120_deg)) ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitY,   90_deg)) ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitY, - 90_deg)) ==
			        ( Vec3r{0,  0,  1}));
			REQUIRE(((Vec3r{5,  0,  0}) * xen::Rotation3d(Vec3r::UnitY, - 90_deg)) ==
			        ( Vec3r{0,  0,  5}));
			REQUIRE(((Vec3r{1,  5,  0}) * xen::Rotation3d(Vec3r::UnitY,  270_deg)) ==
			        ( Vec3r{0,  5,  1}));
			REQUIRE(((Vec3r{0,  0,  1}) * xen::Rotation3d(Vec3r::UnitY,  180_deg)) ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{1,  5,  3}) * xen::Rotation3d(Vec3r::UnitY,   60_deg)) ==
			        ( Vec3r{3.098076, 5, 0.633975}));

			//THEN("Quaternion Matricies equivalent to Rotation Matricies"){
			//	REQUIRE((xen::Rotation3dy(  0_deg)) == (xen::Rotation3d(Vec3r::UnitY,   0_deg)));
			//	REQUIRE((xen::Rotation3dy( 90_deg)) == (xen::Rotation3d(Vec3r::UnitY,  90_deg)));
			//	REQUIRE((xen::Rotation3dy(180_deg)) == (xen::Rotation3d(Vec3r::UnitY, 180_deg)));
			//	REQUIRE((xen::Rotation3dy(270_deg)) == (xen::Rotation3d(Vec3r::UnitY, 270_deg)));
			//}
		}
	}

	SECTION("Rot Z"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  0,  3}) * xen::Rotation3d(Vec3r::UnitZ,  45_deg))  ==
			        ( Vec3r{0,  0,  3}));
			REQUIRE(((Vec3r{0,  0, -4}) * xen::Rotation3d(Vec3r::UnitZ,  90_deg))  ==
			        ( Vec3r{0,  0, -4}));
			REQUIRE(((Vec3r{0,  0, 10}) * xen::Rotation3d(Vec3r::UnitZ, 220_deg))  ==
			        ( Vec3r{0,  0, 10}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{1,  0,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, -90_deg))  ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{3,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, -90_deg))  ==
			        ( Vec3r{0,  3,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, 270_deg))  ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitZ, 180_deg))  ==
			        ( Vec3r{0, -1,  0}));

			//THEN("Quaternion Matricies equivalent to Rotation Matricies"){
			//	REQUIRE((xen::Rotation3dz(  0_deg)) == (xen::Rotation3d(Vec3r::UnitZ,   0_deg)));
			//	REQUIRE((xen::Rotation3dz( 90_deg)) == (xen::Rotation3d(Vec3r::UnitZ,  90_deg)));
			//	REQUIRE((xen::Rotation3dz(180_deg)) == (xen::Rotation3d(Vec3r::UnitZ, 180_deg)));
			//	REQUIRE((xen::Rotation3dz(270_deg)) == (xen::Rotation3d(Vec3r::UnitZ, 270_deg)));
			//}
		}
	}

	// Expected results calculated with:
	// http://www.nh.cas.cz/people/lazar/celler/online_tools.php
	SECTION("Arbitary Rotations"){
		REQUIRE((Vec3r{ 0, 0, 0 } * xen::Rotation3d({1,1,1}, 45_deg)) ==
		        (Vec3r{ 0, 0, 0 }));
		REQUIRE((Vec3r{ 1, 0, 0 } * xen::Rotation3d({1,1,1}, 45_deg)) ==
		        (Vec3r{ 0.804738,0.505879,-0.310617 }));
		REQUIRE((Vec3r{ 1,10, 0} * xen::Rotation3d({1,7,5}, 60_deg)) ==
		        (Vec3r{-4.026667,8.813333,2.666667 }));
		REQUIRE((Vec3r{ 6, 5, 4} * xen::Rotation3d({3,2,1}, 200_deg)) ==
		        (Vec3r{ 7.388367,4.717156,0.400586 }));
	}
}
*/
