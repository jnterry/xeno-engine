////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::Aabb
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/geometry.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Aabb Comparison Operators", "[math][Aabb]"){
	CHECK(operator==(xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}}
	                ) == true
	      );
	CHECK(operator==(xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::Aabb2r{Vec2r{5,2}, Vec2r{3,4}}
	                ) == false
	      );
	CHECK(operator==(xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::Aabb2r{Vec2r{1,5}, Vec2r{3,4}}
	                ) == false
	      );
	CHECK(operator==(xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::Aabb2r{Vec2r{1,2}, Vec2r{5,4}}
	                ) == false
	      );
	CHECK(operator==(xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::Aabb2r{Vec2r{1,2}, Vec2r{3,5}}
	                ) == false
	      );


	CHECK(operator!=(xen::Aabb2s{Vec2s{1,5}, Vec2s{8,9}},
	                 xen::Aabb2s{Vec2s{8,9}, Vec2s{1,5}}
	                ) == true
	      );
	CHECK(operator!=(xen::Aabb2u{Vec2u{0,0}, Vec2u{1,2}},
	                 xen::Aabb2u{Vec2u{0,0}, Vec2u{1,2}}
	                ) == false
	      );

	CHECK(operator==(xen::Aabb3r{Vec3r{1,2,3}, Vec3r{4,5,6}},
	                 xen::Aabb3r{Vec3r{1,2,3}, Vec3r{4,5,6}}
	                ) == true
	      );

	CHECK(operator!=(xen::Aabb3u{Vec3u{1,2,3}, Vec3u{4,5,6}},
	                 xen::Aabb3u{Vec3u{1,4,3}, Vec3u{4,5,6}}
	                ) == true
	      );
}

TEST_CASE("Aabb2/Aabb2 Intersection", "[math][Aabb]"){
	SECTION("Has intersection"){
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{10, 5}},
		                           xen::Aabb2r{Vec2r{ 1, 2}, Vec2r{ 3, 4}})
		      ==
		      xen::Aabb2r{Vec2r{ 1, 2}, Vec2r{ 3, 4}}
		      );

		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 2, 3}, Vec2r{ 4, 5}},
		                           xen::Aabb2r{Vec2r{ 2, 3}, Vec2r{ 4, 5}})
		      ==
		      xen::Aabb2r{Vec2r{ 2, 3}, Vec2r{ 4, 5}}
		      );

		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{10, 5}},
		                           xen::Aabb2r{Vec2r{ 1, 2}, Vec2r{ 3, 4}})
		      ==
		      xen::Aabb2r{Vec2r{ 1, 2}, Vec2r{ 3, 4}}
		      );

		// 1 Edge intersecting
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 1, 1}},
		                           xen::Aabb2r{Vec2r{ 0, 1}, Vec2r{ 1, 2}})
		      ==
		      xen::Aabb2r{Vec2r{ 0, 1}, Vec2r{ 1, 1}}
		      );

		// part of edge intersecting
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 2, 2}},
		                           xen::Aabb2r{Vec2r{ 1, 2}, Vec2r{ 3, 4}})
		      ==
		      xen::Aabb2r{Vec2r{ 1, 2}, Vec2r{ 2, 2}}
		      );

		// 1 corner intersecting
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 1, 1}},
		                           xen::Aabb2r{Vec2r{ 1, 1}, Vec2r{ 2, 2}})
		      ==
		      xen::Aabb2r{Vec2r{ 1, 1}, Vec2r{ 1, 1}}
		      );
	}


	SECTION("No intersection"){
		xen::Aabb2r lhs;
		xen::Aabb2r rhs;
		xen::Aabb2r result;

		// RHS to the left
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{10, 5}, Vec2r{11, 6}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the top
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{ 5,10}, Vec2r{ 6,11}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the right
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{ 0, 5}, Vec2r{ 1, 6}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the bottom
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{ 5, 0}, Vec2r{ 6, 1}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the top-left
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{10,10}, Vec2r{11,11}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the top-right
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{ 0,10}, Vec2r{ 1,11}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the bottom-left
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{10, 0}, Vec2r{11, 1}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the bottom-right
		lhs = xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}};
		rhs = xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 1, 1}};
		CHECK(xen::haveIntersection(lhs, rhs) == false);
		CHECK(xen::intersect       (lhs, rhs) == false);
		CHECK(lhs == (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));
	}
}

TEST_CASE("Aabb Swizzles", "[math][Aabb][swizzle]"){
	SECTION("To 2d"){
		CHECK(xen::swizzle<'x', 'y'>(xen::Aabb3r{Vec3r{1,2,3}, Vec3r{4,5,6}}) ==
		      xen::Aabb2r{Vec2r{1,2}, Vec2r{4,5}}
		     );
		CHECK(xen::swizzle<'y', 'x'>(xen::Aabb3r{Vec3r{1,2,3}, Vec3r{4,5,6}}) ==
		      xen::Aabb2r{Vec2r{2,1}, Vec2r{5,4}}
		     );
	}

	SECTION("To 3d"){
		CHECK(xen::swizzle<'x','y','x'>(xen::Aabb2r{Vec2r{1,2}, Vec2r{3,4}}) ==
		      xen::Aabb3r{Vec3r{1,2,1}, Vec3r{3,4,3}}
		     );
	}
}

TEST_CASE("computeAabb", "[math][Aabb]"){

	SECTION("2d unit axes") {
		CHECK(xen::computeBoundingBox(Vec2r::UnitAxes, 2) == xen::Aabb2r{{0,0}, {1,1}});
	}

	SECTION("3d unit axes") {
		CHECK(xen::computeBoundingBox(Vec3r::UnitAxes, 3) == xen::Aabb3r{{0,0,0}, {1,1,1}});
	}

	SECTION("4d unit axes") {
		CHECK(xen::computeBoundingBox(Vec4r::UnitAxes, 4) == xen::Aabb4r{{0,0,0,0}, {1,1,1,1}});
	}

	SECTION("3d arbitrary") {
		Vec3r ps[] = {
			Vec3r{- 5,  10,  20},
			Vec3r{  1,   3,   8},
			Vec3r{  1,  25, - 7},
			Vec3r{  6,  -2, - 6},
			Vec3r{  0,  -4,   9},
		};
		CHECK(xen::computeBoundingBox(ps, 3) == xen::Aabb3r{{-5, 3,-7}, {1,25,20}});
		CHECK(xen::computeBoundingBox(ps, 5) == xen::Aabb3r{{-5,-4,-7}, {6,25,20}});
	}
}


// :TODO: aabb3 intersections
// :TODO: translations
// :TODO: transformations
// :TODO: area
