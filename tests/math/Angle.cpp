////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Angle.cpp
/// \author Jamie Terry
/// \date 10/06/2017
/// \brief Contains unit tests for xen::Angle
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/angle.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Creating Angles", "[math][Angle]"){
	REQUIRE( 10_deg == xen::Degrees    ( 10));
	REQUIRE(100_deg == xen::Degrees    (100));
	REQUIRE( 10_rad == xen::Radians    ( 10));
	REQUIRE(100_rad == xen::Radians    (100));
	REQUIRE( 10_rev == xen::Revolutions( 10));
	REQUIRE(100_rev == xen::Revolutions(100));

	REQUIRE(360_deg == 1_rev);
	REQUIRE(360_deg == xen::Radians(2.0_r * xen::PI));
	REQUIRE(360_deg == 6.28318530718_rad);
	REQUIRE(90_deg  == 0.25_rev);
}

TEST_CASE("Angle Equality and Clamps", "[math][Angle]"){
	CHECK( 90_deg ==              90_deg );
	CHECK( 90_deg == xen::clamp(  90_deg));

	CHECK(180_deg ==             180_deg );
	CHECK(180_deg == xen::clamp( 180_deg));

	CHECK(270_deg ==             270_deg );
	CHECK(270_deg == xen::clamp( 270_deg));

	CHECK(180_deg !=             540_deg );
	CHECK(180_deg == xen::clamp( 540_deg));

	CHECK(270_deg !=            - 90_deg );
	CHECK(270_deg == xen::clamp(- 90_deg));

	CHECK( 90_deg !=            -270_deg );
	CHECK( 90_deg == xen::clamp(-270_deg));

	CHECK(180_deg !=            -180_deg );
	CHECK(180_deg == xen::clamp(-180_deg));

	CHECK(180_deg !=            -540_deg );
	CHECK(180_deg == xen::clamp(-540_deg));
}

TEST_CASE("Angle Trig", "[math][Angle]"){
	CHECK(xen::sin(-360_deg) == Approx( 0            ));
	CHECK(xen::sin(-270_deg) == Approx( 1            ));
	CHECK(xen::sin(-200_deg) == Approx( 0.34202014332));
	CHECK(xen::sin(-180_deg) == Approx( 0            ));
	CHECK(xen::sin(- 90_deg) == Approx(-1            ));
	CHECK(xen::sin(- 60_deg) == Approx(-0.86602540378));
	CHECK(xen::sin(- 45_deg) == Approx(-0.70710678118));
	CHECK(xen::sin(   0_deg) == Approx( 0            ));
	CHECK(xen::sin(  45_deg) == Approx( 0.70710678118));
	CHECK(xen::sin(  60_deg) == Approx( 0.86602540378));
	CHECK(xen::sin(  90_deg) == Approx( 1            ));
	CHECK(xen::sin( 180_deg) == Approx( 0            ));
	CHECK(xen::sin( 200_deg) == Approx(-0.34202014332));
	CHECK(xen::sin( 270_deg) == Approx(-1            ));
	CHECK(xen::sin( 360_deg) == Approx( 0            ));

	CHECK(xen::cos(-360_deg) == Approx( 1            ));
	CHECK(xen::cos(-270_deg) == Approx( 0            ));
	CHECK(xen::cos(-200_deg) == Approx(-0.93969262078));
	CHECK(xen::cos(-180_deg) == Approx(-1            ));
	CHECK(xen::cos(- 90_deg) == Approx( 0            ));
	CHECK(xen::cos(- 60_deg) == Approx( 0.5          ));
	CHECK(xen::cos(- 45_deg) == Approx( 0.70710678118));
	CHECK(xen::cos(   0_deg) == Approx( 1            ));
	CHECK(xen::cos(  45_deg) == Approx( 0.70710678118));
	CHECK(xen::cos(  60_deg) == Approx( 0.5          ));
	CHECK(xen::cos(  90_deg) == Approx( 0            ));
	CHECK(xen::cos( 180_deg) == Approx(-1            ));
	CHECK(xen::cos( 200_deg) == Approx(-0.93969262078));
	CHECK(xen::cos( 270_deg) == Approx( 0            ));
	CHECK(xen::cos( 360_deg) == Approx( 1            ));

	CHECK(xen::tan(-360_deg) == Approx( 0            ));
	//CHECK(xen::tan(-270_deg) == Approx(              ));
	CHECK(xen::tan(-200_deg) == Approx(-0.36397023426));
	CHECK(xen::tan(-180_deg) == Approx( 0            ));
	//CHECK(xen::tan(- 90_deg) == Approx(              ));
	CHECK(xen::tan(- 60_deg) == Approx(-1.73205080757));
	CHECK(xen::tan(- 45_deg) == Approx(-1            ));
	CHECK(xen::tan(   0_deg) == Approx( 0            ));
	CHECK(xen::tan(  45_deg) == Approx( 1            ));
	CHECK(xen::tan(  60_deg) == Approx( 1.73205080757));
	//CHECK(xen::tan(  90_deg) == Approx(              ));
	CHECK(xen::tan( 180_deg) == Approx( 0            ));
	CHECK(xen::tan( 200_deg) == Approx( 0.36397023426));
	//CHECK(xen::tan( 270_deg) == Approx(              ));
	CHECK(xen::tan( 360_deg) == Approx( 0            ));
}
