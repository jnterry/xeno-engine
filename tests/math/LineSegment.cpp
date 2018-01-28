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

}
