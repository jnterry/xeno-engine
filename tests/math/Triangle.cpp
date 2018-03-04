#include <xen/math/vertex_group.hpp>
#include <xen/math/geometry.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Comparison Operators", "[math][Triangle]"){
	CHECK(operator==(xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{1, 0}},
	                 xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{1, 0}}
	                 ) == true
	     );

	CHECK(operator==(xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{1, 0}},
	                 xen::Triangle2r{Vec2r{0,0}, Vec2r{0,1}, Vec2r{0, 1}}
	                 ) == false
	      );

	CHECK(operator!=(xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{1, 0}},
	                 xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{1, 0}}
	                 ) == false
	     );

	CHECK(operator!=(xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{1, 0}},
	                 xen::Triangle2r{Vec2r{0,0}, Vec2r{0,1}, Vec2r{0, 1}}
	                 ) == true
	     );
}


TEST_CASE("Transform", "[math][Triangle]"){
	CHECK(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}} * xen::Rotation3dz(90_deg)
	      ==
	      xen::Triangle3r{Vec3r{0,0,0}, Vec3r{0,1,0}, Vec3r{-1,0,0}}
	     );

	CHECK(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}} * xen::Rotation3dy(180_deg)
	      ==
	      xen::Triangle3r{Vec3r{0,0,0}, Vec3r{-1,0,0}, Vec3r{0,1,0}}
	     );

	CHECK(xen::Triangle2r{Vec2r{1,1}, Vec2r{2,1}, Vec2r{1,2}} * xen::Scale2d(5, 2)
	      ==
	      xen::Triangle2r{Vec2r{5,2}, Vec2r{10,2}, Vec2r{5,4}}
	     );
}

TEST_CASE("Triangle fromHomo", "[math][Triangle]"){
	CHECK(xen::fromHomo(xen::Triangle4r{Vec4r{0,0,0,1}, Vec4r{1,0,0,1}, Vec4r{0,1,0,1}})
	      ==
	      xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}}
	     );

	CHECK(xen::fromHomo(xen::Triangle3r{Vec3r{0,0,1}, Vec3r{2,0,1}, Vec3r{0,3,1}})
	      ==
	      xen::Triangle2r{Vec2r{0,0}, Vec2r{2,0}, Vec2r{0,3}}
	     );

	CHECK(xen::fromHomo(xen::Triangle3r{Vec3r{0,0,2}, Vec3r{2,0,2}, Vec3r{0,3,1}})
	      ==
	      xen::Triangle2r{Vec2r{0,0}, Vec2r{1,0}, Vec2r{0,3}}
	     );
}

TEST_CASE("Triangle toHomo", "[math][Triangle]"){
	CHECK(xen::toHomo(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}})
	      ==
	      xen::Triangle4r{Vec4r{0,0,0,1}, Vec4r{1,0,0,1}, Vec4r{0,1,0,1}}
	     );

	CHECK(xen::toHomo(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}},2_r)
	      ==
	      xen::Triangle4r{Vec4r{0,0,0,2}, Vec4r{1,0,0,2}, Vec4r{0,1,0,2}}
	     );

	CHECK(xen::toHomo(xen::Triangle2r{Vec2r{0,0}, Vec2r{2,0}, Vec2r{0,3}})
	      ==
	      xen::Triangle3r{Vec3r{0,0,1}, Vec3r{2,0,1}, Vec3r{0,3,1}}
	     );
	}

	TEST_CASE("Triangle getBarycentric", "[math][Triangle]"){
		CHECK(xen::getBarycentricCoordinates(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}},
		                                     Vec3r{0.5_r, 0.2_r, 0}
																			  )
		      ==
		      Vec3r{0.3_r, 0.5_r, 0.2_r}
		     );

		CHECK(xen::getBarycentricCoordinates(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}},
		                                     Vec3r{0,0,0}
																				)
					==
					Vec3r{1.0_r, 0.0, 0.0}
				 );
		CHECK(xen::getBarycentricCoordinates(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}},
		                                     Vec3r{1,0,0}
																				)
					==
					Vec3r{0.0_r, 1.0_r, 0.0_r}
				 );

		CHECK(xen::getBarycentricCoordinates(xen::Triangle3r{Vec3r{0,0,0}, Vec3r{1,0,0}, Vec3r{0,1,0}},
		                                     Vec3r{0,1,0}
																				)
					==
					Vec3r{0.0_r, 0.0_r, 1.0_r}
				 );
}
