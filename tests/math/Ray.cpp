#include <xen/math/geometry.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Ray Comparison Operators", "[math][Ray]"){
	CHECK(operator==(xen::Ray2r{Vec2r{0,0}, Vec2r{1,0}},
	                 xen::Ray2r{Vec2r{0,0}, Vec2r{1,0}}
	                ) == true
	     );
	CHECK(operator==(xen::Ray2r{Vec2r{5,7}, Vec2r{0,1}},
	                 xen::Ray2r{Vec2r{5,7}, Vec2r{0,1}}
	                ) == true
	     );
	CHECK(operator==(xen::Ray2r{Vec2r{5,7}, Vec2r{1,0}},
	                 xen::Ray2r{Vec2r{5,7}, Vec2r{0,1}}
	                ) == false
	     );

	CHECK(operator==(xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,0,1}},
	                 xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,0,1}}
	                ) == true
	     );

	CHECK(operator==(xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,0,1}},
	                 xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,1,0}}
	                 ) == false
	     );

	CHECK(operator!=(xen::Ray2r{Vec2r{1,2}, Vec2r{1,0}},
	                 xen::Ray2r{Vec2r{1,4}, Vec2r{1,0}}
	                ) == true
	     );
	CHECK(operator!=(xen::Ray2r{Vec2r{5,4}, Vec2r{0,1}},
	                 xen::Ray2r{Vec2r{1,4}, Vec2r{0,1}}
	                ) == true
	     );
	CHECK(operator!=(xen::Ray2r{Vec2r{8,9}, Vec2r{1,0}},
	                 xen::Ray2r{Vec2r{8,9}, Vec2r{0,1}}
	                ) == true
	     );
	CHECK(operator!=(xen::Ray3r{Vec3r{2,4,6}, Vec3r{1,0,0}},
	                 xen::Ray3r{Vec3r{2,4,6}, Vec3r{1,0,0}}
	                 ) == false
	     );
	CHECK(operator!=(xen::Ray3r{Vec3r{2,4,6}, Vec3r{1,0,0}},
	                 xen::Ray3r{Vec3r{2,4,7}, Vec3r{1,0,0}}
	                 ) == true
	      );
	CHECK(operator!=(xen::Ray3r{Vec3r{2,4,8}, Vec3r{1,0,0}},
	                 xen::Ray3r{Vec3r{2,4,8}, Vec3r{0,0,1}}
	                 ) == true
	      );
}

TEST_CASE("Ray Translate", "[math][Ray]"){
	CHECK(xen::getTranslated(xen::Ray2r{Vec2r{0,0}, Vec2r{1,0}},
	                         Vec2r{1,9}
	                        )
	      ==
	      xen::Ray2r{Vec2r{1,9}, Vec2r{1,0}}
	     );

	CHECK(xen::getTranslated(xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,0,1}},
	                         Vec3r{4,1,2}
	                        )
	      ==
	      xen::Ray3r{Vec3r{5,3,5}, Vec3r{0,0,1}}
	     );
}

TEST_CASE("Ray Transform 2d", "[math][Ray]"){
	CHECK(xen::getTransformed(xen::Ray2r{Vec2r{0,0}, Vec2r{1,0}},
	                          xen::Scale2d(3,3)
	                          )
	      ==
	      xen::Ray2r{Vec2r{0,0}, Vec2r{1,0}}
	     );

	CHECK(xen::getTransformed(xen::Ray2r{Vec2r{1,0}, Vec2r{1,0}},
	                          xen::Scale2d(3,3)
	                          )
	      ==
	      xen::Ray2r{Vec2r{3,0}, Vec2r{1,0}}
	     );

	/*CHECK(xen::getTransformed(xen::Ray2r{Vec2r{1,0}, Vec2r{1,0}},
	                          xen::Translate2d(2,4)
	                         )
	      ==
	      xen::Ray2r{Vec2r{3,4}, Vec2r{1,0}}
	      );*/
}
