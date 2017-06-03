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
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitX, 90_deg) == (Quat{COS_45, 0, 0, COS_45}));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitY, 90_deg) == (Quat{0, COS_45, 0, COS_45}));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitZ, 90_deg) == (Quat{0, 0, COS_45, COS_45}));
	}

	SECTION("0 Deg rotation about each axis"){
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitX, 0_deg) == (Quat::Identity));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitY, 0_deg) == (Quat::Identity));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitZ, 0_deg) == (Quat::Identity));
	}
}


TEST_CASE("AxisAngle -> Quaternion -> AxisAngle results in no change",
          "[math][Quaternion][AxisAngle]"){
	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r::UnitX,  30_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitX,  30_deg}));

	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r::UnitY, 120_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitY, 120_deg}));

	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r::UnitZ, 250_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitZ, 250_deg}));

	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r{0, 0.5, 1}, 45_deg))) ==
	        (xen::AxisAngle{xen::normalized(Vec3r{0, 0.5, 1}), 45_deg}));
}

TEST_CASE("Rotation Matrix from Quaterion (xen::Rotation3d())",
          "[math][Quaternion][AxisAngle]"){

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
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, -90_deg))  ==
			        ( Vec3r{0,  0,  1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 270_deg))  ==
			        ( Vec3r{0,  0,  1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 180_deg))  ==
			        ( Vec3r{0, -1,  0}));

			THEN("Quaternion Matricies equivalent to Rotation Matricies"){
				REQUIRE((xen::Rotation3dx(  0_deg)) == (xen::Rotation3d(Vec3r::UnitX,   0_deg)));
				REQUIRE((xen::Rotation3dx( 90_deg)) == (xen::Rotation3d(Vec3r::UnitX,  90_deg)));
				REQUIRE((xen::Rotation3dx(180_deg)) == (xen::Rotation3d(Vec3r::UnitX, 180_deg)));
				REQUIRE((xen::Rotation3dx(270_deg)) == (xen::Rotation3d(Vec3r::UnitX, 270_deg)));
			}
		}
	}

	SECTION("Rot Y"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitY, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitY, 90_deg))  ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitY, -90_deg))  ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{1,  5,  0}) * xen::Rotation3d(Vec3r::UnitY, 270_deg))  ==
			        ( Vec3r{0,  5, -1}));
			REQUIRE(((Vec3r{0,  0,  1}) * xen::Rotation3d(Vec3r::UnitY, 180_deg))  ==
			        ( Vec3r{0,  0, -1}));

			THEN("Quaternion Matricies equivalent to Rotation Matricies"){
				REQUIRE((xen::Rotation3dy(  0_deg)) == (xen::Rotation3d(Vec3r::UnitY,   0_deg)));
				REQUIRE((xen::Rotation3dy( 90_deg)) == (xen::Rotation3d(Vec3r::UnitY,  90_deg)));
				REQUIRE((xen::Rotation3dy(180_deg)) == (xen::Rotation3d(Vec3r::UnitY, 180_deg)));
				REQUIRE((xen::Rotation3dy(270_deg)) == (xen::Rotation3d(Vec3r::UnitY, 270_deg)));
			}
		}
	}

	SECTION("Rot Z"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  0, 10}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{0,  0, 10}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{1,  0,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, -90_deg))  ==
			        ( Vec3r{0, -1,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, 270_deg))  ==
			        ( Vec3r{0, -1,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitZ, 180_deg))  ==
			        ( Vec3r{0, -1,  0}));

			THEN("Quaternion Matricies equivalent to Rotation Matricies"){
				REQUIRE((xen::Rotation3dz(  0_deg)) == (xen::Rotation3d(Vec3r::UnitZ,   0_deg)));
				REQUIRE((xen::Rotation3dz( 90_deg)) == (xen::Rotation3d(Vec3r::UnitZ,  90_deg)));
				REQUIRE((xen::Rotation3dz(180_deg)) == (xen::Rotation3d(Vec3r::UnitZ, 180_deg)));
				REQUIRE((xen::Rotation3dz(270_deg)) == (xen::Rotation3d(Vec3r::UnitZ, 270_deg)));
			}
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
