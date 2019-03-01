////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::Vector
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/vector.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Vec3r, Comparison Operators", "[math][Vector]"){
	CHECK(operator==(Vec3r{1,1,1}, Vec3r{1,1,1}) == true );
	CHECK(operator!=(Vec3r{1,1,1}, Vec3r{1,1,1}) == false);

	CHECK(operator==(Vec3r{1,1, 1.1_r}, Vec3r{1,1, 0.9_r + 0.2_r}) == true );
	CHECK(operator!=(Vec3r{1,1, 1.1_r}, Vec3r{1,1, 0.9_r + 0.2_r}) == false);

	CHECK(operator==(Vec3r{1,1,1.001_r}, Vec3r{1,1,1}) == false);
	CHECK(operator!=(Vec3r{1,1,1.001_r}, Vec3r{1,1,1}) == true );
}

TEST_CASE("Vec3r Length/Magnitude/Distance/Squared", "[math][Vector]"){
	CHECK(xen::length  (Vec3r{5,0,0}) ==  5);
	CHECK(xen::mag     (Vec3r{5,0,0}) ==  5);
	CHECK(xen::lengthSq(Vec3r{5,0,0}) == 25);
	CHECK(xen::magSq   (Vec3r{5,0,0}) == 25);

	CHECK(xen::length  (Vec3r{0,3,0}) == 3);
	CHECK(xen::mag     (Vec3r{0,3,0}) == 3);
	CHECK(xen::lengthSq(Vec3r{0,3,0}) == 9);
	CHECK(xen::magSq   (Vec3r{0,3,0}) == 9);

	CHECK(xen::length  (Vec3r{0,0,4}) ==  4);
	CHECK(xen::mag     (Vec3r{0,0,4}) ==  4);
	CHECK(xen::lengthSq(Vec3r{0,0,4}) == 16);
	CHECK(xen::magSq   (Vec3r{0,0,4}) == 16);

	CHECK(xen::length  (Vec3r{3,0,4}) ==  5);
	CHECK(xen::mag     (Vec3r{3,0,4}) ==  5);
	CHECK(xen::lengthSq(Vec3r{3,0,4}) == 25);
	CHECK(xen::magSq   (Vec3r{3,0,4}) == 25);

	CHECK(xen::distance  (Vec3r{1,0,0}, Vec3r{-5, 0, 0}) ==  6);
	CHECK(xen::distanceSq(Vec3r{1,0,0}, Vec3r{-5, 0, 0}) == 36);

	CHECK(xen::distance  (Vec3r{1,-2,0}, Vec3r{5,-5,0}) ==  5);
	CHECK(xen::distanceSq(Vec3r{1,-2,0}, Vec3r{5,-5,0}) == 25);
}

TEST_CASE("Vec3r Angle Between", "[math][Vector]"){
	SECTION("Normalized"){
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 1, 0, 0})) ==   0_deg);
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{-1, 0, 0})) == 180_deg);
	}

	SECTION("Non-Normalized"){
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 5, 0, 0})) ==   0_deg);
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{-5, 0, 0})) == 180_deg);
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 0, 1, 0})) ==  90_deg);
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 0, 0, 1})) ==  90_deg);
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 1, 0, 1})) ==  45_deg);
		CHECK((xen::angleBetween(Vec3r{ 1, 0, 0}, Vec3r{ 2, 0, 0})) ==   0_deg);

		// https://www.emathhelp.net/calculators/linear-algebra/angle-between-two-vectors-calculator/?ux=1&uy=2&uz=3&vx=4&vy=5&vz=6&steps=on
		CHECK((xen::angleBetween(Vec3r{  1,  2,  3}, Vec3r{  4,  5,  6})) ==   12.933154491_deg);
		CHECK((xen::angleBetween(Vec3r{ 10, 11, 12}, Vec3r{ 10, 11, 13})) ==   2.2581483623_deg);
	}
}

TEST_CASE("Vec3r Dot Product", "[math][Vector]"){

	SECTION("Orthoganal Vectors have dot of 0"){
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r(0,1,0)) == Approx(0));
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r(0,0,1)) == Approx(0));
		CHECK(xen::dot(Vec3r(0,1,0), Vec3r(0,0,1)) == Approx(0));

		CHECK(xen::dot(Vec3r(3,0,0), Vec3r(0,3,0)) == Approx(0));
		CHECK(xen::dot(Vec3r(6,0,0), Vec3r(0,0,6)) == Approx(0));
		CHECK(xen::dot(Vec3r(0,8,0), Vec3r(0,0,8)) == Approx(0));

		CHECK(xen::dot(Vec3r(3,0,0), Vec3r(0,5,0)) == Approx(0));
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r(0,0,4)) == Approx(0));
		CHECK(xen::dot(Vec3r(0,2,0), Vec3r(0,0,7)) == Approx(0));
	}

	SECTION("Opposite Vectors have dot of -1"){
		CHECK(xen::dot(Vec3r(0,1,0), Vec3r( 0,-1, 0)) == Approx(-1));
		CHECK(xen::dot(xen::normalized(Vec3r(3,0,0)), xen::normalized(Vec3r(-3, 0, 0))) == Approx(-1));
		CHECK(xen::dot(xen::normalized(Vec3r(0,0,7)), xen::normalized(Vec3r( 0, 0,-7))) == Approx(-1));
	}

	SECTION("Equal Vectors have dot of 1"){
		CHECK(xen::dot(Vec3r(1,0,0), Vec3r( 1, 0, 0)) == Approx( 1));
		CHECK(xen::dot(xen::normalized(Vec3r(3,0,0)), xen::normalized(Vec3r( 3, 0, 0))) == Approx( 1));
		CHECK(xen::dot(xen::normalized(Vec3r(3,2,0)), xen::normalized(Vec3r( 3, 2, 0))) == Approx( 1));
	}


	SECTION("Arbitary Vectors"){
	        CHECK(xen::dot(Vec3r(1,2,3)  , Vec3r(5,9,-2)) == Approx(17));
	        CHECK(xen::dot(Vec3r(-10,6,8), Vec3r(2,-3,9)) == Approx(34));
	}
}


TEST_CASE("Vec3r Project onto Plane",  "[math][Vector]"){
	SECTION("Onto XY"){
		for(real x = -5; x <= 5; ++x){
			for(real y = -5; y <= 5; ++y){
				for(real z = -5; z <= 5; ++z){
					CHECK(xen::projectOntoPlane<real>({x,y,z},  Vec3r::UnitZ) == Vec3r(x,y,0));
					CHECK(xen::projectOntoPlane<real>({x,y,z}, -Vec3r::UnitZ) == Vec3r(x,y,0));
				}
			}
		}
	}

	SECTION("Onto XZ"){
		for(real x = -5; x <= 5; ++x){
			for(real y = -5; y <= 5; ++y){
				for(real z = -5; z <= 5; ++z){
					CHECK(xen::projectOntoPlane<real>({x,y,z},  Vec3r::UnitY) == Vec3r(x,0,z));
					CHECK(xen::projectOntoPlane<real>({x,y,z}, -Vec3r::UnitY) == Vec3r(x,0,z));
				}
			}
		}
	}

	SECTION("Onto YZ"){
		for(real x = -5; x <= 5; ++x){
			for(real y = -5; y <= 5; ++y){
				for(real z = -5; z <= 5; ++z){
					CHECK(xen::projectOntoPlane<real>({x,y,z},  Vec3r::UnitX) == Vec3r(0,y,z));
					CHECK(xen::projectOntoPlane<real>({x,y,z}, -Vec3r::UnitX) == Vec3r(0,y,z));
				}
			}
		}
	}

	//:TODO: onto arbitary planes
}

TEST_CASE("Vector toHomo", "[math][Vector]"){
	CHECK(xen::toHomo(Vec3r{1,2,3}     ) == Vec4r{1,2,3,1});
	CHECK(xen::toHomo(Vec3r{1,2,3}, 0_r) == Vec4r{1,2,3,0});

	CHECK(xen::toHomo(Vec2r{6,7  }     ) == Vec3r{6,7,1  });
	CHECK(xen::toHomo(Vec2r{8,9  }, 0_r) == Vec3r{8,9,0  });
}

TEST_CASE("Vector fromHomo", "[math][Vector]"){
	CHECK(xen::fromHomo(Vec4r{2,4,6,1})  == Vec3r{2,4,6});
	CHECK(xen::fromHomo(Vec4r{2,4,6,2})  == Vec3r{1,2,3});

	CHECK(xen::fromHomo(Vec3r{4,8,1  })  == Vec2r{4,8  });
	CHECK(xen::fromHomo(Vec3r{4,8,2  })  == Vec2r{2,4  });
}

TEST_CASE("Elementwise min-max", "[math][minmax][Vector]"){
	SECTION("Two way"){
		CHECK(xen::min(Vec2r{ 1,  2}, Vec2r{ 3,  4}) == Vec2r{ 1, 2});
		CHECK(xen::min(Vec2r{ 1, 10}, Vec2r{ 5,  7}) == Vec2r{ 1, 7});
		CHECK(xen::min(Vec2r{ 8, 10}, Vec2r{ 5,  7}) == Vec2r{ 5, 7});
		CHECK(xen::min(Vec2r{ 8,  6}, Vec2r{ 5,  7}) == Vec2r{ 5, 6});

		CHECK(xen::min(Vec3r{1, 2, 3}, Vec3r{-1, 5, 2}) == Vec3r{-1, 2, 2});

		CHECK(xen::min(Vec4r{1, 2, 3, 10}, Vec4r{-1, 5, 2, 5}) == Vec4r{-1, 2, 2, 5});



		CHECK(xen::max(Vec2r{ 1,  2}, Vec2r{ 3,  4}) == Vec2r{ 3, 4});
		CHECK(xen::max(Vec2r{ 1, 10}, Vec2r{ 5,  7}) == Vec2r{ 5,10});
		CHECK(xen::max(Vec2r{ 8, 10}, Vec2r{ 5,  7}) == Vec2r{ 8,10});
		CHECK(xen::max(Vec2r{ 8,  6}, Vec2r{ 5,  7}) == Vec2r{ 8, 7});

		CHECK(xen::max(Vec3r{1, 2, 3}, Vec3r{-1, 5, 2}) == Vec3r{1, 5, 3});

		CHECK(xen::max(Vec4r{1, 2, 3, 5}, Vec4r{-1, 5, 2, 5}) == Vec4r{1, 5, 3, 5});
	}

	// :TODO: this doesn't work...
	//SECTION("Three way"){
	//	CHECK(xen::min(Vec3r{1, 10, 5}, Vec3r{3, 3, 7}, Vec3r{8, 20, 2}) == Vec3r{1, 3, 2});
	//	CHECK(xen::max(Vec3r{1, 10, 5}, Vec3r{3, 3, 7}, Vec3r{8, 20, 2}) == Vec3r{8, 20, 7});
	//}
}

TEST_CASE("Vector Swizzles", "[math][Vector][swizzle]"){
	SECTION("To 2d"){
		CHECK(xen::swizzle<'y', 'x'>(Vec2r{1,2  }) == Vec2r{2, 1});
		CHECK(xen::swizzle<'x', 'x'>(Vec2r{5,6  }) == Vec2r{5, 5});
		CHECK(xen::swizzle<'x', 'z'>(Vec3r{3,2,1}) == Vec2r{3, 1});
		CHECK(xen::swizzle<'y', 'x'>(Vec3r{3,2,1}) == Vec2r{2, 3});
	}

	SECTION("To 3d"){
		CHECK(xen::swizzle<'x', 'y', 'x'>(Vec2r{5,8    }) == Vec3r(5,8,5));
		CHECK(xen::swizzle<'x', 'z', 'y'>(Vec3r{1,2,3  }) == Vec3r(1,3,2));
		CHECK(xen::swizzle<'x', 'y', 'w'>(Vec4r{1,2,3,4}) == Vec3r(1,2,4));
	}

	SECTION("To 4d"){
		CHECK(xen::swizzle<'x', 'y', 'y', 'x'>(Vec2r{5,8    }) == Vec4r(5,8,8,5));
		CHECK(xen::swizzle<'z', 'y', 'x', 'x'>(Vec3r{5,6,7  }) == Vec4r(7,6,5,5));
		CHECK(xen::swizzle<'y', 'x', 'z', 'w'>(Vec4r{1,2,3,4}) == Vec4r(2,1,3,4));
	}
}
