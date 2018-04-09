////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::Ray
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

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

	CHECK(xen::getTransformed(xen::Ray2r{Vec2r{1,0}, Vec2r{1,0}},
	                          xen::Translation2d(2,4)
	                          )
	      ==
	      xen::Ray2r{Vec2r{3,4}, Vec2r{1,0}}
	     );

	CHECK(xen::getTransformed(xen::Ray2r{Vec2r{0,0}, Vec2r{1,0}},
	                          xen::Rotation2d(90_deg)
	                          )
	      ==
	      xen::Ray2r{Vec2r{0,0}, Vec2r{0,-1}}
	     );

	CHECK(xen::getTransformed(xen::Ray2r{Vec2r{0,5}, Vec2r{1,0}},
	                          xen::Rotation2d(180_deg)
	                          )
	      ==
	      xen::Ray2r{Vec2r{0,-5}, Vec2r{-1,0}}
	     );
}

TEST_CASE("Ray Transform 3d", "[math][Ray]"){
	CHECK(xen::getTransformed(xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,0,1}},
	                          xen::Scale3d(4,5,6)
	                          )
	      ==
	      xen::Ray3r{Vec3r{4,10,18}, Vec3r{0,0,1}}
	     );

	CHECK(xen::getTransformed(xen::Ray3r{Vec3r{1,2,3}, Vec3r{0,1,0}},
	                          xen::Translation3d(4,5,6)
	                          )
	      ==
	      xen::Ray3r{Vec3r{5,7,9}, Vec3r{0,1,0}}
	     );

	CHECK(xen::getTransformed(xen::Ray3r{Vec3r{5,0,0}, Vec3r{1,0,0}},
	                          xen::Rotation3dx(180_deg)
	                          )
	      ==
	      xen::Ray3r{Vec3r{5,0,0}, Vec3r{1,0,0}}
	     );

	CHECK(xen::getTransformed(xen::Ray3r{Vec3r{5,0,0}, Vec3r{0,1,0}},
	                          xen::Rotation3dy(-90_deg)
	                          )
	      ==
	      xen::Ray3r{Vec3r{0,0,5}, Vec3r{0,1,0}}
	     );

	CHECK(xen::getTransformed(xen::Ray3r{Vec3r{3,0,4}, Vec3r{0,1,0}},
	                          xen::Rotation3dz(90_deg)
	                          )
	      ==
	      xen::Ray3r{Vec3r{0,3,4}, Vec3r{-1,0,0}}
	     );
}
