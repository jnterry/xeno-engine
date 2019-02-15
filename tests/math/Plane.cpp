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

	CHECK(xen::getSignedDistanceToPlane({  Vec3r::UnitX,   0 }, pos) ==  3);
	CHECK(xen::getSignedDistanceToPlane({  Vec3r::UnitY,   0 }, pos) ==  4);
	CHECK(xen::getSignedDistanceToPlane({  Vec3r::UnitZ,   0 }, pos) ==  5);
	CHECK(xen::getSignedDistanceToPlane({ -Vec3r::UnitX,   0 }, pos) == -3);
	CHECK(xen::getSignedDistanceToPlane({ -Vec3r::UnitY,   0 }, pos) == -4);
	CHECK(xen::getSignedDistanceToPlane({ -Vec3r::UnitZ,   0 }, pos) == -5);

	CHECK(xen::distanceToPlane         ({  Vec3r::UnitX,   0 }, pos) == 3);
	CHECK(xen::distanceToPlane         ({  Vec3r::UnitY,   0 }, pos) == 4);
	CHECK(xen::distanceToPlane         ({  Vec3r::UnitZ,   0 }, pos) == 5);

	CHECK(xen::distanceToPlane         ({ -Vec3r::UnitX,   0 }, pos) == 3);
	CHECK(xen::distanceToPlane         ({ -Vec3r::UnitY,   0 }, pos) == 4);
	CHECK(xen::distanceToPlane         ({ -Vec3r::UnitZ,   0 }, pos) == 5);

	CHECK(xen::getSignedDistanceToPlane({  Vec3r::UnitZ,  10 }, pos) == 15);
	CHECK(xen::getSignedDistanceToPlane({  Vec3r::UnitZ, -10 }, pos) == -5);

	CHECK(xen::getSignedDistanceToPlane({ -Vec3r::UnitZ,  10 }, pos) ==   5);
	CHECK(xen::getSignedDistanceToPlane({ -Vec3r::UnitZ, -10 }, pos) == -15);
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

TEST_CASE("Closest Point on Rhombus", "[math][Plane]"){

	xen::PlaneRhombus3r r = { Vec3r::Origin, Vec3r::UnitX, Vec3r::UnitY };

	// Points on bound vertices
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.0_r, 0.0_r, 0.0_r}) == Vec3r{ 0.0_r, 0.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.0_r, 1.0_r, 0.0_r}) == Vec3r{ 0.0_r, 1.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 1.0_r, 1.0_r, 0.0_r}) == Vec3r{ 1.0_r, 1.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 1.0_r, 0.0_r, 0.0_r}) == Vec3r{ 1.0_r, 0.0_r, 0.0_r});

	// Points on boundary lines
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.5_r, 0.0_r, 0.0_r}) == Vec3r{ 0.5_r, 0.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 1.0_r, 0.5_r, 0.0_r}) == Vec3r{ 1.0_r, 0.5_r, 0.0_r});

	// Points within area
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.3_r, 0.7_r, 0.0_r}) == Vec3r{ 0.3_r, 0.7_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.5_r, 0.5_r, 0.0_r}) == Vec3r{ 0.5_r, 0.5_r, 0.0_r});

	// Points on plane outside area
	CHECK(xen::getClosestPoint(r, Vec3r{ 2.0_r, 0.0_r, 0.0_r}) == Vec3r{ 1.0_r, 0.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 2.0_r, 2.0_r, 0.0_r}) == Vec3r{ 1.0_r, 1.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.0_r, 2.0_r, 0.0_r}) == Vec3r{ 0.0_r, 1.0_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.5_r, 2.0_r, 0.0_r}) == Vec3r{ 0.5_r, 1.0_r, 0.0_r});

	// Points within area but off plane
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.5_r, 0.5_r,  1.0_r}) == Vec3r{ 0.5_r, 0.5_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.3_r, 0.8_r,  8.0_r}) == Vec3r{ 0.3_r, 0.8_r, 0.0_r});
	CHECK(xen::getClosestPoint(r, Vec3r{ 0.1_r, 0.2_r, -5.0_r}) == Vec3r{ 0.1_r, 0.2_r, 0.0_r});
}

TEST_CASE("Get Transformed", "[math][Plane]") {
	// Translation within plane is no-op
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(0,  1,  0)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 3 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(0, 10,  0)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 3 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(0,  0,  1)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 3 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(0, 10, 10)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 3 });

	// Scale within plane is no-op
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Scale3d(1,  5,  1)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 3 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Scale3d(1,  3,  5)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 3 });

	// Translation along normal
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(-1, 0, 0)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 4 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(1, 0, 0)) ==
	                          xen::PlaneEquation<real>{ Vec3r::UnitX, 2 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Translation3d(2, 5, 6)) ==
	                          xen::PlaneEquation<real>{ Vec3r::UnitX, 1 });

	// Scale along normal
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Scale3d(2, 1, 1)) ==
	      xen::PlaneEquation<real>{ Vec3r::UnitX, 6 });
	CHECK(xen::getTransformed({ Vec3r::UnitX, 3 }, xen::Scale3d(-1, 1, 1)) ==
	      xen::PlaneEquation<real>{ -Vec3r::UnitX, 3 });
}
