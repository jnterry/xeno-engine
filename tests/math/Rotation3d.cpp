////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for the various ways by which rotations can be
/// applied to a 3d point
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/vector_types.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/quaternion.hpp>

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
	REQUIRE(rot.elements[ 3] == Approx(0)); \
	REQUIRE(rot.elements[ 7] == Approx(0)); \
	REQUIRE(rot.elements[11] == Approx(0)); \
	REQUIRE(rot.elements[12] == Approx(0)); \
	REQUIRE(rot.elements[13] == Approx(0)); \
	REQUIRE(rot.elements[14] == Approx(0)); \
	REQUIRE(rot.elements[15] == Approx(1)); \
	REQUIRE(xen::determinant(rot) == Approx(1.0_r));

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
			for(real a_val = -100; a_val <= 100; a_val += 50){
				Vec3r p(0, 0, 0);
				p.elements[axis] = a_val;
				Mat4r rot;
				for(xen::Angle a = -300_deg; a <= 300_deg; a += 50_deg){
					switch(axis){
					case 0: rot = xen::Rotation3dx(a); break;
					case 1: rot = xen::Rotation3dy(a); break;
					case 2: rot = xen::Rotation3dz(a); break;
					}
					REQUIRE_IS_ROT_MAT(rot);
					REQUIRE((p * rot) == p);

					REQUIRE(xen::rotated(p, Vec3r::UnitAxes[axis], a) == p);

					Mat4r rotA = xen::Rotation3d(Vec3r::UnitAxes[axis], a );
					Mat4r rotB = xen::Rotation3d(xen::AxisAngle(Vec3r::UnitAxes[axis], a));;
					REQUIRE(rotA == rotB);
					REQUIRE_IS_ROT_MAT(rotA);
					REQUIRE((p * rotA) == p);
					Quat q = Quat(Vec3r::UnitAxes[axis], a);
					REQUIRE(xen::mag(q) == Approx(1.0_r));
					rot = xen::Rotation3d(q);
					REQUIRE_IS_ROT_MAT(rot);
					REQUIRE((p * rot) == p);
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
						for(xen::Angle a = -300_deg; a <= 300_deg; a += 50_deg){
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

#define STRINGIFY(x) #x
#define TO_STRING(x) STRINGIFY(x)
#define GEN_NAME(px,py,pz, ex,ey,ez, a) TO_STRING(__LINE__) ": p = ("  TO_STRING(px) "," TO_STRING(py) "," TO_STRING(pz) "), angle = " TO_STRING(a)

TEST_CASE("Rotating about X Axis"){
	// expected results calculated with:
	// http://www.nh.cas.cz/people/lazar/celler/online_tools.php

	REQUIRE(Vec3r::UnitX == Vec3r(1,0,0));

#define CHECK_ROTS_X_SINGLE(px,py,pz, ex,ey,ez, a)	  \
	WHEN(GEN_NAME(px,py,pz, ex,ey,ez, a)){ \
		Vec3r p((px), (py), (pz)); \
		Vec3r e((ex), (ey), (ez)); \
	    CHECK(xen::rotated(p,                 Vec3r::UnitX, a ) == e); \
		CHECK(xen::rotated(p, xen::AxisAngle {Vec3r::UnitX, a}) == e); \
		CHECK(xen::rotated(p, xen::Quaternion(Vec3r::UnitX, a)) == e); \
		CHECK  ((p * xen::Rotation3dx(                              a)) == e); \
		REQUIRE((p * xen::Rotation3d (                Vec3r::UnitX, a) ) == e); \
		CHECK  ((p * xen::Rotation3d (xen::AxisAngle {Vec3r::UnitX, a})) == e); \
		CHECK  ((p * xen::Rotation3d (xen::Quaternion(Vec3r::UnitX, a))) == e); \
	}
#define CHECK_ROTS_X(px,py,pz, ex,ey,ez, a)	  \
	for(xen::Angle da = -1_rev; da <= 1_rev; da += 1_rev){ \
		for(real f = -8; f <= 7; f += 3){ \
			CHECK_ROTS_X_SINGLE((px),f*(py),f*(pz),    (ex),f*(ey),f*(ez),  (da + a)); \
      } \
	}

	CHECK_ROTS_X(0, 1, 0,     0, 0, 1,      90_deg);
	CHECK_ROTS_X(0, 1, 0,     0, 0,-1,    - 90_deg);
	CHECK_ROTS_X(0, 1, 0,     0,-1, 0,     180_deg);
	CHECK_ROTS_X(0, 1, 0,     0,-1, 0,    -180_deg);

	CHECK_ROTS_X(0, 0, 1,     0,-1, 0,      90_deg);
	CHECK_ROTS_X(0, 0, 1,     0, 1, 0,    - 90_deg);
	CHECK_ROTS_X(0, 0, 1,     0, 0,-1,     180_deg);
	CHECK_ROTS_X(0, 0, 1,     0, 0,-1,    -180_deg);

	CHECK_ROTS_X(3, 6, 2,     3, 4.196152, 4.732051,      30_deg);
	CHECK_ROTS_X(3, 6, 2,     3, 2.828427, 5.656854,      45_deg);
	CHECK_ROTS_X(3, 6, 2,     3, 1.267949, 6.196152,      60_deg);
	CHECK_ROTS_X(3, 6, 2,     3,-2       , 6,             90_deg);
	CHECK_ROTS_X(3, 6, 2,     3,-1.267949,-6.196152,     240_deg);
}

TEST_CASE("Rotating about Y Axis"){
	// expected results calculated with:
	// http://www.nh.cas.cz/people/lazar/celler/online_tools.php

	REQUIRE(Vec3r::UnitY == Vec3r(0,1,0));

#define CHECK_ROTS_Y_SINGLE(px,py,pz, ex,ey,ez, a)	  \
	WHEN(GEN_NAME(px,py,pz, ex,ey,ez, a)){ \
		Vec3r p((px), (py), (pz)); \
		Vec3r e((ex), (ey), (ez)); \
	    CHECK(xen::rotated(p,                 Vec3r::UnitY, a ) == e); \
		CHECK(xen::rotated(p, xen::AxisAngle {Vec3r::UnitY, a}) == e); \
		CHECK(xen::rotated(p, xen::Quaternion(Vec3r::UnitY, a)) == e); \
		CHECK  ((p * xen::Rotation3dy(                              a) ) == e); \
		REQUIRE((p * xen::Rotation3d (                Vec3r::UnitY, a) ) == e); \
		CHECK  ((p * xen::Rotation3d (xen::AxisAngle {Vec3r::UnitY, a})) == e); \
		CHECK  ((p * xen::Rotation3d (xen::Quaternion(Vec3r::UnitY, a))) == e); \
	}
#define CHECK_ROTS_Y(px,py,pz, ex,ey,ez, a)	  \
	for(xen::Angle da = -1_rev; da <= 1_rev; da += 1_rev){ \
		for(real f = -8; f <= 7; f += 3){ \
			CHECK_ROTS_Y_SINGLE(f*(px),(py),f*(pz),    f*(ex),(ey),f*(ez),  (da + a)); \
      } \
	}

	CHECK_ROTS_Y(1, 0, 0,     0, 0,-1,      90_deg);
	CHECK_ROTS_Y(1, 0, 0,     0, 0, 1,    - 90_deg);
	CHECK_ROTS_Y(1, 0, 0,    -1, 0, 0,     180_deg);
	CHECK_ROTS_Y(1, 0, 0,    -1, 0, 0,    -180_deg);

	CHECK_ROTS_Y(0, 0, 1,     1, 0, 0,      90_deg);
	CHECK_ROTS_Y(0, 0, 1,    -1, 0, 0,    - 90_deg);
	CHECK_ROTS_Y(0, 0, 1,     0, 0,-1,     180_deg);
	CHECK_ROTS_Y(0, 0, 1,     0, 0,-1,    -180_deg);

	CHECK_ROTS_Y(3, 6, 2,     3.598076,6,  0.232051,      30_deg);
	CHECK_ROTS_Y(3, 6, 2,     3.535534,6, -0.707107,      45_deg);
	CHECK_ROTS_Y(3, 6, 2,     3.232051,6, -1.598076,      60_deg);
	CHECK_ROTS_Y(3, 6, 2,     2       ,6, -3       ,      90_deg);
	CHECK_ROTS_Y(3, 6, 2,    -3.232051,6,1.598076  ,     240_deg);
}

TEST_CASE("Rotating about Z Axis"){
	// expected results calculated with:
	// http://www.nh.cas.cz/people/lazar/celler/online_tools.php

	REQUIRE(Vec3r::UnitZ == Vec3r(0,0,1));

#define CHECK_ROTS_Z_SINGLE(px,py,pz, ex,ey,ez, a)	  \
	WHEN(GEN_NAME(px,py,pz, ex,ey,ez, a)){ \
		Vec3r p((px), (py), (pz)); \
		Vec3r e((ex), (ey), (ez)); \
	    CHECK(xen::rotated(p,                 Vec3r::UnitZ, a ) == e); \
		CHECK(xen::rotated(p, xen::AxisAngle {Vec3r::UnitZ, a}) == e); \
		CHECK(xen::rotated(p, xen::Quaternion(Vec3r::UnitZ, a)) == e); \
		CHECK  ((p * xen::Rotation3dz(                              a) ) == e); \
		REQUIRE((p * xen::Rotation3d (                Vec3r::UnitZ, a) ) == e); \
		CHECK  ((p * xen::Rotation3d (xen::AxisAngle {Vec3r::UnitZ, a})) == e); \
		CHECK  ((p * xen::Rotation3d (xen::Quaternion(Vec3r::UnitZ, a))) == e); \
	}
#define CHECK_ROTS_Z(px,py,pz, ex,ey,ez, a)	  \
	for(xen::Angle da = -1_rev; da <= 1_rev; da += 1_rev){ \
		for(real f = -8; f <= 7; f += 3){ \
			CHECK_ROTS_Z_SINGLE(f*(px),f*(py),(pz),    f*(ex),f*(ey),(ez),  (da + a)); \
      } \
	}

	CHECK_ROTS_Z(1, 0, 0,     0, 1, 0,      90_deg);
	CHECK_ROTS_Z(1, 0, 0,     0,-1, 0,    - 90_deg);
	CHECK_ROTS_Z(1, 0, 0,    -1, 0, 0,     180_deg);
	CHECK_ROTS_Z(1, 0, 0,    -1, 0, 0,    -180_deg);

	CHECK_ROTS_Z(0, 1, 0,    -1, 0, 0,      90_deg);
	CHECK_ROTS_Z(0, 1, 0,     1, 0, 0,    - 90_deg);
	CHECK_ROTS_Z(0, 1, 0,     0,-1, 0,     180_deg);
	CHECK_ROTS_Z(0, 1, 0,     0,-1, 0,    -180_deg);

	CHECK_ROTS_Z(3, 6, 2,    -0.401924, 6.696152,2,    30_deg);
	CHECK_ROTS_Z(3, 6, 2,    -2.12132 , 6.363961,2,    45_deg);
	CHECK_ROTS_Z(3, 6, 2,    -3.696152, 5.598076,2,    60_deg);
	CHECK_ROTS_Z(3, 6, 2,    -6       , 3       ,2,    90_deg);
    CHECK_ROTS_Z(3, 6, 2,     3.696152,-5.598076,2,   240_deg);
}


TEST_CASE("Quaternion to Matrix", "[math][Quaternion]"){
	// expected results calculated with: http://www.energid.com/resources/orientation-calculator/
	// (note these results transposed when compared to ours, due to diff in row-major vs column-major order)

#define TEST_QUAT_TO_MAT(ax, ay, az, a, m0,m1,m2,m3,m4,m5,m6,m7,m8)	  \
	WHEN("Axis: (" #ax "," #ay "," #az "), Angle = " #a){ \
		Mat4r mat = xen::Rotation3d(Vec3r(ax,ay,az), a); \
		REQUIRE_IS_ROT_MAT(mat); \
		\
		Mat4r mat_aa = xen::Rotation3d(xen::AxisAngle{Vec3r(ax,ay,az), a}); \
		REQUIRE_THAT(mat_aa, IsMat(mat)); \
		\
		Mat4r mat_q = xen::Rotation3d(Quat(Vec3r(ax,ay,az), a)); \
		REQUIRE_THAT(mat_q, IsMat(mat)); \
		\
		Mat4r mat_q_mod = xen::Rotation3d(Quat(5*Vec3r(ax,ay,az), a + 360_deg)); \
	    CHECK_THAT(mat_q_mod, IsMat(mat)); \
		\
	    Mat4r mat_q_neg = xen::Rotation3d(Quat(-1*Vec3r(ax,ay,az), -1*a)); \
	    CHECK_THAT(mat_q_neg, IsMat(mat)); \
	    \
		CHECK_THAT(mat, IsMat(Mat4r{ m0,m1,m2,0,  m3,m4,m5,0,  m6,m7,m8,0,  0,0,0,1 })); \
	}

	TEST_QUAT_TO_MAT(1,2,5, 30_deg,
	                  0.8704911151974474_r ,  0.46536728922051085_r, -0.1602451387276938_r ,
	                 -0.44750399476498637_r,  0.8838885860390907_r ,  0.13594536453736095_r,
	                  0.20490337486650506_r, -0.04662889225973847_r,  0.9776708819305944_r
	                 );

	TEST_QUAT_TO_MAT(6,-2,3, -50_deg,
	                  0.9052292898242471_r , -0.4157855199139829_r, -0.0876489262578162_r,
	                  0.24082420882028527_r,  0.671947541699317_r , -0.7003500565076926_r,
	                  0.3500908928983626_r ,  0.6128694009608437_r,  0.7083978148438373_r
	                 );

	TEST_QUAT_TO_MAT(5,1,0, 60_deg,
	                  0.9807692158201847_r ,  0.09615392089907657_r, -0.16984159913046662_r,
	                  0.09615392089907657_r,  0.5192303955046171_r ,  0.8492079956523331_r ,
	                  0.16984159913046662_r, -0.8492079956523331_r ,  0.49999961132480186_r
	                 );
}

TEST_CASE("Quaternion and Matrix rotations equivalent"){
	SECTION("X Axis"){
		CHECK_THAT(xen::Rotation3dx(   0_deg), IsMat(xen::Rotation3d(Vec3r::UnitX,    0_deg)));
		CHECK_THAT(xen::Rotation3dx(  30_deg), IsMat(xen::Rotation3d(Vec3r::UnitX,   30_deg)));
		CHECK_THAT(xen::Rotation3dx(  45_deg), IsMat(xen::Rotation3d(Vec3r::UnitX,   45_deg)));
		CHECK_THAT(xen::Rotation3dx(  60_deg), IsMat(xen::Rotation3d(Vec3r::UnitX,   60_deg)));
		CHECK_THAT(xen::Rotation3dx(  90_deg), IsMat(xen::Rotation3d(Vec3r::UnitX,   90_deg)));
		CHECK_THAT(xen::Rotation3dx( 120_deg), IsMat(xen::Rotation3d(Vec3r::UnitX,  120_deg)));
		CHECK_THAT(xen::Rotation3dx(-100_deg), IsMat(xen::Rotation3d(Vec3r::UnitX, -100_deg)));
	}

	SECTION("Y Axis"){
		CHECK_THAT(xen::Rotation3dy(   0_deg), IsMat(xen::Rotation3d(Vec3r::UnitY,    0_deg)));
		CHECK_THAT(xen::Rotation3dy(  30_deg), IsMat(xen::Rotation3d(Vec3r::UnitY,   30_deg)));
		CHECK_THAT(xen::Rotation3dy(  45_deg), IsMat(xen::Rotation3d(Vec3r::UnitY,   45_deg)));
		CHECK_THAT(xen::Rotation3dy(  60_deg), IsMat(xen::Rotation3d(Vec3r::UnitY,   60_deg)));
		CHECK_THAT(xen::Rotation3dy(  90_deg), IsMat(xen::Rotation3d(Vec3r::UnitY,   90_deg)));
		CHECK_THAT(xen::Rotation3dy( 120_deg), IsMat(xen::Rotation3d(Vec3r::UnitY,  120_deg)));
		CHECK_THAT(xen::Rotation3dy(-100_deg), IsMat(xen::Rotation3d(Vec3r::UnitY, -100_deg)));
	}

	SECTION("Z Axis"){
		CHECK_THAT(xen::Rotation3dz(   0_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ,    0_deg)));
		CHECK_THAT(xen::Rotation3dz(  30_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ,   30_deg)));
		CHECK_THAT(xen::Rotation3dz(  45_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ,   45_deg)));
		CHECK_THAT(xen::Rotation3dz(  60_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ,   60_deg)));
		CHECK_THAT(xen::Rotation3dz(  90_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ,   90_deg)));
		CHECK_THAT(xen::Rotation3dz( 120_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ,  120_deg)));
		CHECK_THAT(xen::Rotation3dz(-100_deg), IsMat(xen::Rotation3d(Vec3r::UnitZ, -100_deg)));
	}
}
