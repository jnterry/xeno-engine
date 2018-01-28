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
		// RHS to the left
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{10, 5}, Vec2r{11, 6}})
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the top
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{ 5,10}, Vec2r{ 6,11}})
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the right
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{ 0, 5}, Vec2r{ 1, 6}})
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the bottom
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{ 5, 0}, Vec2r{ 6, 1}})
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the top-left
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{10,10}, Vec2r{11,11}}
		                           )
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the top-right
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{ 0,10}, Vec2r{ 1,11}}
		                           )
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the bottom-left
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{10, 0}, Vec2r{11, 1}}
		                           )
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));

		// RHS to the bottom-right
		CHECK(xen::getIntersection(xen::Aabb2r{Vec2r{ 5, 5}, Vec2r{ 6, 6}},
		                           xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 1, 1}}
		                           )
		      ==
		      (xen::Aabb2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}}));
	}
}
