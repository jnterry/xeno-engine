#include <xen/math/geometry.hpp>
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
		ray.origin = Vec3r{0.2, 0.2, 1};
		CHECK(xen::getIntersection(ray, tri, result) == true);
		CHECK(result == Vec3r{0.2, 0.2, 0});

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
		ray.origin = Vec3r{0.2, 0.2, 1};
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
