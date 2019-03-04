////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for testing for ray intersections with other
/// geometry types
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/geometry.hpp>
#include <xen/math/plane.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Ray Triangle Intersection 3d", "[math][Ray][Triangle]"){
	xen::Ray3r      ray;
	xen::Triangle3r tri;
	Vec3r result;

	tri.p1 = Vec3r{0, 0, 0};
	tri.p2 = Vec3r{1, 0, 0};
	tri.p3 = Vec3r{0, 1, 0};

	ray.direction = Vec3r{1, 0, 0};

	// We set result to 5,7,8 to test if it is modified when there is no
	// intersection -> it shouldn't be

	SECTION("Ray in triangle plane"){
		// Ray in same plane as triangle, and hits triangle
		// :TODO: currently we take this to NOT intersect (due to
		// algorithm used, not by choice) -> what is the best choice?

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{-1, 0, 0};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		//CHECK(result == Vec3r{0,0,0});
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{-5, 0, 0};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		//CHECK(result == Vec3r{0,0,0});
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{-5, 0.5, 0};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		//CHECK(result == Vec3r{0,0.5,0});
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{-10, 1, 0};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		//CHECK(result == Vec3r{0,1,0});
		CHECK(result == Vec3r{5,7,8});
	}


	SECTION("Ray parallel with triangle normal") {
		ray.direction = Vec3r{0, 0, -1};

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{0.2_r, 0.2_r, 1};
		CHECK(xen::getIntersection(ray, tri, result) == true);
		CHECK(result == Vec3r{0.2_r, 0.2_r, 0});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{0, 0, 1};
		CHECK(xen::getIntersection(ray, tri, result) == true);
		CHECK(result == Vec3r{0, 0, 0});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{1, 0, 1};
		CHECK(xen::getIntersection(ray, tri, result) == true);
		CHECK(result == Vec3r{1, 0, 0});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{0, 1, 1};
		CHECK(xen::getIntersection(ray, tri, result) == true);
		CHECK(result == Vec3r{0, 1, 0});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{1, 1, 1};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		CHECK(result == Vec3r{5, 7, 8});
	}

	SECTION("Triangle behind ray") {
		ray.direction = Vec3r{0, 0, 1};

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{0.2_r, 0.2_r, 1};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{0, 0, 1};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{1, 0, 1};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{0, 1, 1};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		CHECK(result == Vec3r{5,7,8});

		result = Vec3r{5,7,8};
		ray.origin = Vec3r{1, 1, 1};
		CHECK(xen::getIntersection(ray, tri, result) == false);
		CHECK(result == Vec3r{5,7,8});
	}
}

TEST_CASE("getClosestPointOnRay", "[math][Ray][Vector]"){
	SECTION("Ray from origin in x dir"){
		xen::Ray3r r;
		r.origin    = Vec3r::Origin;
		r.direction = Vec3r::UnitX;

		CHECK(xen::getClosestPointOnRay(r, Vec3r{-1.0_r,  0.0_r,  0.0_r}) ==
		      Vec3r::Origin);
		CHECK(xen::getClosestPointOnRay(r, Vec3r{ 0.0_r,  0.0_r,  0.0_r}) ==
		      Vec3r::Origin);
		CHECK(xen::getClosestPointOnRay(r, Vec3r{ 1.0_r,  0.0_r,  0.0_r}) ==
		      Vec3r::UnitX);
		CHECK(xen::getClosestPointOnRay(r, Vec3r{ 1.0_r,  1.0_r,  0.0_r}) ==
		      Vec3r::UnitX);
		CHECK(xen::getClosestPointOnRay(r, Vec3r{ 1.0_r,  5.0_r,  0.0_r}) ==
		      Vec3r::UnitX);
		CHECK(xen::getClosestPointOnRay(r, Vec3r{ 1.0_r,  1.0_r,  1.0_r}) ==
		      Vec3r::UnitX);
	}
}

TEST_CASE("getDistanceSqBetweenRayAndPoint", "[math][Ray][Vector]"){
	SECTION("Ray from origin in x dir"){
		xen::Ray3r r;
		r.origin    = Vec3r::Origin;
		r.direction = Vec3r::UnitX;

		CHECK(xen::getDistanceSqBetweenRayAndPoint(r, Vec3r{-1.0_r,  0.0_r,  0.0_r}) ==  1);
		CHECK(xen::getDistanceSqBetweenRayAndPoint(r, Vec3r{ 0.0_r,  0.0_r,  0.0_r}) ==  0);
		CHECK(xen::getDistanceSqBetweenRayAndPoint(r, Vec3r{ 1.0_r,  0.0_r,  0.0_r}) ==  0);
		CHECK(xen::getDistanceSqBetweenRayAndPoint(r, Vec3r{ 1.0_r,  1.0_r,  0.0_r}) ==  1);
		CHECK(xen::getDistanceSqBetweenRayAndPoint(r, Vec3r{ 1.0_r,  5.0_r,  0.0_r}) == 25);
		CHECK(xen::getDistanceSqBetweenRayAndPoint(r, Vec3r{ 1.0_r,  1.0_r,  1.0_r}) ==  2);
	}
}

TEST_CASE("getIntersection plane and ray", "[math][Ray][Plane]"){
  Vec3r result;
  xen::Plane3r plane;
  xen::Ray3r   ray;

  plane = { { 1, 0, 0 }, { 1, 0, 0 } };
  ray   = { { 0, 0, 0 }, { 1, 0, 0 } };
	CHECK(xen::getIntersection(plane, ray, result) == true);
	CHECK(result == Vec3r{ 1, 0, 0 });

	ray   = { { 0, 5, 3 }, { 1, 0, 0 } };
	CHECK(xen::getIntersection(plane, ray, result) == true);
	CHECK(result == Vec3r{ 1, 5, 3 });

	ray   = { { -5, 10, 0 }, { 1, 0, 0 } };
	CHECK(xen::getIntersection(plane, ray, result) == true);
	CHECK(result == Vec3r{ 1, 10, 0 });

	result = { 1, 2, 3 };
	ray   = { { 0, 0, 0 }, { -1, 0, 0 } };
	CHECK(xen::getIntersection(plane, ray, result) == false);
	CHECK(result == Vec3r{ 1, 2, 3 });
}
