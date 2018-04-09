////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Unit tests for misc mathematical utility functions
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/intrinsics.hpp>
#include <xen/math/utilities.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>


TEST_CASE("linear interpolation"){
	SECTION("lerp", "[math][utilities][lerp]"){
		CHECK(xen::lerp(0_r,  1_r, 0.3_r) ==  0.3_r);
		CHECK(xen::lerp(0_r, 10_r, 0.3_r) ==  3.0_r);

		CHECK(xen::lerp(5_r, 10_r, 0.0_r) ==  5.0_r);
		CHECK(xen::lerp(5_r, 10_r, 0.5_r) ==  7.5_r);
		CHECK(xen::lerp(5_r, 10_r, 1.0_r) == 10.0_r);
		CHECK(xen::lerp(5_r, 10_r, 2.0_r) == 15.0_r);
	}

	SECTION("invLerp", "[math][utilities][lerp]"){
		CHECK(xen::invLerp(0_r,  1_r, 0.3_r)  ==  0.3_r);
		CHECK(xen::invLerp(0_r, 10_r, 3.0_r)  ==  0.3_r);

		CHECK(xen::invLerp(5_r, 10_r,  5.0_r) ==  0.0_r);
		CHECK(xen::invLerp(5_r, 10_r,  7.5_r) ==  0.5_r);
		CHECK(xen::invLerp(5_r, 10_r, 10.0_r) ==  1.0_r);
		CHECK(xen::invLerp(5_r, 10_r, 15.0_r) ==  2.0_r);
	}

	SECTION("mapToRange", "[math][utilities][lerp]"){
		CHECK(xen::mapToRange<float, u08>(0.0f, 1.0f, 0, 255, 0.0f) ==   0);
		CHECK(xen::mapToRange<float, u08>(0.0f, 1.0f, 0, 255, 0.5f) == 127);
		CHECK(xen::mapToRange<float, u08>(0.0f, 1.0f, 0, 255, 1.0f) == 255);

		CHECK(xen::mapToRange<u08, float>(0, 255, 0.0f, 1.0f,   0) == 0.0f);
		CHECK(xen::mapToRange<u08, float>(0, 255, 0.0f, 1.0f, 100) == 100.0f / 255.0f);
		CHECK(xen::mapToRange<u08, float>(0, 255, 0.0f, 1.0f, 127) == 127.0f / 255.0f);
		CHECK(xen::mapToRange<u08, float>(0, 255, 0.0f, 1.0f, 200) == 200.0f / 255.0f);
		CHECK(xen::mapToRange<u08, float>(0, 255, 0.0f, 1.0f, 255) == 1.0f);

		CHECK(xen::mapToRange(0.0f, 10.0f, 5.0f, 6.0f, -10.0f) == 4.00f);
		CHECK(xen::mapToRange(0.0f, 10.0f, 5.0f, 6.0f,   0.0f) == 5.00f);
		CHECK(xen::mapToRange(0.0f, 10.0f, 5.0f, 6.0f,   5.5f) == 5.55f);
		CHECK(xen::mapToRange(0.0f, 10.0f, 5.0f, 6.0f,  10.0f) == 6.00f);
		CHECK(xen::mapToRange(0.0f, 10.0f, 5.0f, 6.0f,  20.0f) == 7.00f);
	}

	SECTION("mapToRangeClamped", "[math][utilities][lerp]"){
		CHECK(xen::mapToRangeClamped(0.0f, 10.0f, 5.0f, 6.0f, -10.0f) == 5.00f);
		CHECK(xen::mapToRangeClamped(0.0f, 10.0f, 5.0f, 6.0f,   0.0f) == 5.00f);
		CHECK(xen::mapToRangeClamped(0.0f, 10.0f, 5.0f, 6.0f,   5.5f) == 5.55f);
		CHECK(xen::mapToRangeClamped(0.0f, 10.0f, 5.0f, 6.0f,  10.0f) == 6.00f);
		CHECK(xen::mapToRangeClamped(0.0f, 10.0f, 5.0f, 6.0f,  20.0f) == 6.00f);
	}
}
