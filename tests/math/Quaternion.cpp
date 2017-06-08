#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Quaternion.hpp>

#include "ostream_operators.hpp"

#include <catch.hpp>

#define COS_45 0.70710678118

TEST_CASE("Making Quaternion from AxisAngle",
          "[math][Quaternion][AxisAngle]"){

	REQUIRE( Quat::Identity == (Quat{0,0,0,1}) );

	SECTION("90 Deg rotation about each axis"){
		REQUIRE(Quat(Vec3r::UnitX, 90_deg) == (Quat{COS_45, 0, 0, COS_45}));
		REQUIRE(Quat(Vec3r::UnitY, 90_deg) == (Quat{0, COS_45, 0, COS_45}));
		REQUIRE(Quat(Vec3r::UnitZ, 90_deg) == (Quat{0, 0, COS_45, COS_45}));
	}

	SECTION("0 Deg rotation about each axis"){
		REQUIRE(Quat(Vec3r::UnitX, 0_deg) == (Quat::Identity));
		REQUIRE(Quat(Vec3r::UnitY, 0_deg) == (Quat::Identity));
		REQUIRE(Quat(Vec3r::UnitZ, 0_deg) == (Quat::Identity));
	}
}

TEST_CASE("AxisAngle -> Quaternion -> AxisAngle results in no change",
          "[math][Quaternion][AxisAngle]"){
	REQUIRE((xen::AxisAngle(Quat(Vec3r::UnitX,  30_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitX,  30_deg}));

	REQUIRE((xen::AxisAngle(Quat(Vec3r::UnitY, 120_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitY, 120_deg}));

	REQUIRE((xen::AxisAngle(Quat(Vec3r::UnitZ, 250_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitZ, 250_deg}));

	REQUIRE((xen::AxisAngle(Quat(Vec3r{0, 0.5, 1}, 45_deg))) ==
	        (xen::AxisAngle{xen::normalized(Vec3r{0, 0.5, 1}), 45_deg}));
}

TEST_CASE("Quaterion * Quaterion", "[math][Quaternion][AxisAngle]"){
	// expected results calculated with http://www.bluetulip.org/2014/programs/quaternions.html
	REQUIRE(((Quat{ 1,-3,-6, 3  }) * (Quat{ 7, 9, 3, 2})) == (Quat{68  ,-24, 27  , 44}));
	REQUIRE(((Quat{ 9, 8, 7, 0  }) * (Quat{ 2, 3, 4, 1})) == (Quat{20  ,-14, 18  ,-70}));
	REQUIRE(((Quat{-1, 0, 0, 1.5}) * (Quat{ 3, 2,-7, 0})) == (Quat{ 4.5,- 4,-12.5,  3}));
}

TEST_CASE("Quaternion Magnitude", "[math][Quaternion]"){
	REQUIRE(xen::mag(Quat( 0, 0, 0, 0)) == 0);
	REQUIRE(xen::mag(Quat( 1, 1, 1, 1)) == 2);
	REQUIRE(xen::mag(Quat( 8, 6, 1, 1)) == xen::sqrt(64 + 36 + 1 + 1));
	REQUIRE(xen::mag(Quat( 4, 3, 2, 1)) == xen::sqrt(16 +  9 + 4 + 1));
}

TEST_CASE("Quaternion Normalized", "[math][Quaternion]"){
	//REQUIRE(xen::normalized(Quat(0,0,0,0))   == Quat(0  , 0  , 0   , 0   ));
	REQUIRE(xen::normalized(Quat(1,1,1,1))   == Quat(0.5, 0.5, 0.5 , 0.5 ));
	REQUIRE(xen::normalized(Quat(10,6,-3,4)) == Quat( 0.78811040623,
	                                                  0.47286624374,
	                                                 -0.23643312187,
	                                                  0.31524416249));
	for(real x = -8; x <= 2; x += 4){
		for(real y = -10; y <= 10; y += 10){
			for(real z = -10; z <= 10; z += 10){
				for(real w = -10; w <= 10; w += 3){
					REQUIRE(xen::mag(xen::normalized(Quat(x,y,z,w))) - 1_r <= 0.000001_r);
				}
			}
		}
	}
}

TEST_CASE("getRotation as Quaterion", "[math][Quaternion]"){
	//:TODO: test getting rotation from one vector to another


	/*Quat r = xen::getRotation(Vec3r(1,0,0), Vec3r(0,1,0));
	REQUIRE(1 - mag(r) <= 0.00001_r);
	REQUIRE(((r == Quat( Vec3r::UnitZ, - 90_deg)) ||
	         (r == Quat( Vec3r::UnitZ,  270_deg)) ||
	         (r == Quat(-Vec3r::UnitZ,   90_deg)) ||
	         (r == Quat(-Vec3r::UnitZ, -270_deg))
	         ) == true);*/

}
