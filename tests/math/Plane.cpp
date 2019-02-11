////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::Aabb
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/plane.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Distance from point to plane", "[math][Plane]"){
	Vec3r pos = { 3, 4, 5 };

	CHECK(xen::getDirectionalDistanceToPlane({  Vec3r::UnitX,   0 }, pos) ==  3);
	CHECK(xen::getDirectionalDistanceToPlane({  Vec3r::UnitY,   0 }, pos) ==  4);
	CHECK(xen::getDirectionalDistanceToPlane({  Vec3r::UnitZ,   0 }, pos) ==  5);
	CHECK(xen::getDirectionalDistanceToPlane({ -Vec3r::UnitX,   0 }, pos) == -3);
	CHECK(xen::getDirectionalDistanceToPlane({ -Vec3r::UnitY,   0 }, pos) == -4);
	CHECK(xen::getDirectionalDistanceToPlane({ -Vec3r::UnitZ,   0 }, pos) == -5);

	CHECK(xen::distanceToPlane              ({  Vec3r::UnitX,   0 }, pos) == 3);
	CHECK(xen::distanceToPlane              ({  Vec3r::UnitY,   0 }, pos) == 4);
	CHECK(xen::distanceToPlane              ({  Vec3r::UnitZ,   0 }, pos) == 5);

	CHECK(xen::distanceToPlane              ({ -Vec3r::UnitX,   0 }, pos) == 3);
	CHECK(xen::distanceToPlane              ({ -Vec3r::UnitY,   0 }, pos) == 4);
	CHECK(xen::distanceToPlane              ({ -Vec3r::UnitZ,   0 }, pos) == 5);

	CHECK(xen::getDirectionalDistanceToPlane({  Vec3r::UnitZ,  10 }, pos) == 15);
	CHECK(xen::getDirectionalDistanceToPlane({  Vec3r::UnitZ, -10 }, pos) == -5);

	CHECK(xen::getDirectionalDistanceToPlane({ -Vec3r::UnitZ,  10 }, pos) ==   5);
	CHECK(xen::getDirectionalDistanceToPlane({ -Vec3r::UnitZ, -10 }, pos) == -15);
}

TEST_CASE("Point on plane", "[math][Plane]"){
	Vec3r pos = { 3, 4, 5 };

	CHECK(xen::getPlaneProjection({  Vec3r::UnitX,   0 }, pos) ==  Vec3r{0, 4, 5});
	CHECK(xen::getPlaneProjection({  Vec3r::UnitY,   0 }, pos) ==  Vec3r{3, 0, 5});
	CHECK(xen::getPlaneProjection({  Vec3r::UnitZ,   0 }, pos) ==  Vec3r{3, 4, 0});
	CHECK(xen::getPlaneProjection({ -Vec3r::UnitX,   0 }, pos) ==  Vec3r{0, 4, 5});
	CHECK(xen::getPlaneProjection({ -Vec3r::UnitY,   0 }, pos) ==  Vec3r{3, 0, 5});
	CHECK(xen::getPlaneProjection({ -Vec3r::UnitZ,   0 }, pos) ==  Vec3r{3, 4, 0});

	CHECK(xen::getPlaneProjection({  Vec3r::UnitZ,  10 }, pos) ==  Vec3r{3, 4, -10});
	CHECK(xen::getPlaneProjection({  Vec3r::UnitZ, -10 }, pos) ==  Vec3r{3, 4,  10});
	CHECK(xen::getPlaneProjection({ -Vec3r::UnitZ,  10 }, pos) ==  Vec3r{3, 4,  10});
	CHECK(xen::getPlaneProjection({ -Vec3r::UnitZ, -10 }, pos) ==  Vec3r{3, 4, -10});
}
