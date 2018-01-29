#include <xen/math/geometry.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("LineSegment Comparison Operators", "[math][LineSegment]"){
	CHECK(operator==(xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,4}}
	                 ) == true
	      );
	CHECK(operator==(xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::LineSegment2r{Vec2r{5,2}, Vec2r{3,4}}
	                 ) == false
	      );
	CHECK(operator==(xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::LineSegment2r{Vec2r{1,5}, Vec2r{3,4}}
	                 ) == false
	      );
	CHECK(operator==(xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::LineSegment2r{Vec2r{1,2}, Vec2r{5,4}}
	                 ) == false
	      );
	CHECK(operator==(xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,4}},
	                 xen::LineSegment2r{Vec2r{1,2}, Vec2r{3,5}}
	                 ) == false
	      );


	CHECK(operator!=(xen::LineSegment2s{Vec2s{1,5}, Vec2s{8,9}},
	                 xen::LineSegment2s{Vec2s{8,9}, Vec2s{1,5}}
	                 ) == true
	      );
	CHECK(operator!=(xen::LineSegment2u{Vec2u{0,0}, Vec2u{1,2}},
	                 xen::LineSegment2u{Vec2u{0,0}, Vec2u{1,2}}
	                 ) == false
	      );

	CHECK(operator==(xen::LineSegment3r{Vec3r{1,2,3}, Vec3r{4,5,6}},
	                 xen::LineSegment3r{Vec3r{1,2,3}, Vec3r{4,5,6}}
	                 ) == true
	      );

	CHECK(operator!=(xen::LineSegment3u{Vec3u{1,2,3}, Vec3u{4,5,6}},
	                 xen::LineSegment3u{Vec3u{1,4,3}, Vec3u{4,5,6}}
	                 ) == true
	      );
}

TEST_CASE("LineSegment/Aabb2 Intersection", "[math][Aabb][LineSegment]"){
	xen::LineSegment2r line;
	xen::Aabb2r        aabb;

	SECTION("Have intersection"){
		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 2, 2}};
		line = xen::LineSegment2r{Vec2r{ 0, 1}, Vec2r{ 4, 1}};
		//CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{ 0, 1}, Vec2r{ 2, 1}});

		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 2, 2}};
		line = xen::LineSegment2r{Vec2r{ 1, 0}, Vec2r{ 1, 4}};
		//CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{ 1, 0}, Vec2r{ 1, 2}});

		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 2, 2}};
		line = xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{ 1, 1}};
		//CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{ 1, 1}});
	}

	SECTION("Intersection along edge"){
		// Line longer than Aabb
		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{10,10}};
		line = xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{20, 0}};
		//CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{10, 0}});

		// Line shorter than Aabb
		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{10,10}};
		line = xen::LineSegment2r{Vec2r{10, 3}, Vec2r{10, 7}};
		//CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{10, 3}, Vec2r{10, 7}});
	}

	SECTION("Intersection at corner"){
		// Line starts at Aabb corner
	  aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 1, 1}};
	  line = xen::LineSegment2r{Vec2r{ 1, 1}, Vec2r{ 2, 2}};
	  //CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{ 1, 1}, Vec2r{ 1, 1}});

		// Line ends at Aabb corner
	  aabb = xen::Aabb2r       {Vec2r{-1,-1}, Vec2r{ 1, 1}};
	  line = xen::LineSegment2r{Vec2r{-2,-2}, Vec2r{-1,-1}};
	  //CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{-1,-1}, Vec2r{-1,-1}});

		// Line starts on Aabb edge
	  aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 2, 2}};
	  line = xen::LineSegment2r{Vec2r{ 2, 1}, Vec2r{10, 1}};
	  //CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{ 2, 1}, Vec2r{ 2, 1}});

		// Line touches Aabb corner
	  aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{10,10}};
		line = xen::LineSegment2r{Vec2r{ 0,20}, Vec2r{20, 0}};
		//CHECK(xen::haveIntersection(line, aabb) == true);
		CHECK(xen::intersect       (line, aabb) == true);
		CHECK(line == xen::LineSegment2r{Vec2r{10,10}, Vec2r{10,10}});
	}

	SECTION("No intersection"){
		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 1, 1}};
		line = xen::LineSegment2r{Vec2r{ 0, 2}, Vec2r{ 0, 4}};
		//CHECK(xen::haveIntersection(line, aabb) == false);
		CHECK(xen::intersect       (line, aabb) == false);
		CHECK(line == xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}});

		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{ 1, 1}};
		line = xen::LineSegment2r{Vec2r{ 0, 2}, Vec2r{ 2, 1}};
		//CHECK(xen::haveIntersection(line, aabb) == false);
		CHECK(xen::intersect       (line, aabb) == false);
		CHECK(line == xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}});

		aabb = xen::Aabb2r       {Vec2r{ 0, 0}, Vec2r{10,10}};
		line = xen::LineSegment2r{Vec2r{-9, 9}, Vec2r{ 9,-9}};
		//CHECK(xen::haveIntersection(line, aabb) == false);
		CHECK(xen::intersect       (line, aabb) == false);
		CHECK(line == xen::LineSegment2r{Vec2r{ 0, 0}, Vec2r{ 0, 0}});
	}
}
