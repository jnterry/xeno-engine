////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen camera types and related functions
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/graphics/Image.hpp>
#include "ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Cube Map Direction to Pixel", "[graphics][CubeMap]"){
	CHECK(xen::getCubeMapPixelCoord( Vec3r::UnitX, 3) == Vec3u { 1, 1, xen::CubeMap::Face::PositiveX });
	CHECK(xen::getCubeMapPixelCoord(-Vec3r::UnitX, 3) == Vec3u { 1, 1, xen::CubeMap::Face::NegativeX });
	CHECK(xen::getCubeMapPixelCoord( Vec3r::UnitY, 3) == Vec3u { 1, 1, xen::CubeMap::Face::PositiveY });
	CHECK(xen::getCubeMapPixelCoord(-Vec3r::UnitY, 3) == Vec3u { 1, 1, xen::CubeMap::Face::NegativeY });
	CHECK(xen::getCubeMapPixelCoord( Vec3r::UnitZ, 3) == Vec3u { 1, 1, xen::CubeMap::Face::PositiveZ });
	CHECK(xen::getCubeMapPixelCoord(-Vec3r::UnitZ, 3) == Vec3u { 1, 1, xen::CubeMap::Face::NegativeZ });

	CHECK(xen::getCubeMapPixelCoord({ 0.0, 0.8, 1 }, 3) ==
	      Vec3u { 1, 2, xen::CubeMap::Face::PositiveZ });
	CHECK(xen::getCubeMapPixelCoord({ 0.0, -0.8, 1 }, 3) ==
	      Vec3u { 1, 0, xen::CubeMap::Face::PositiveZ });
	CHECK(xen::getCubeMapPixelCoord({ 0.8, 0.8, 1 }, 3) ==
	      Vec3u { 0, 2, xen::CubeMap::Face::PositiveZ });

	CHECK(xen::getCubeMapPixelCoord({ 0.0, 0.8, -1 }, 3) ==
	      Vec3u { 1, 2, xen::CubeMap::Face::NegativeZ });
	CHECK(xen::getCubeMapPixelCoord({ 0.0, -0.8, -1 }, 3) ==
	      Vec3u { 1, 0, xen::CubeMap::Face::NegativeZ });
}

TEST_CASE("Cube Map Pixel to Direction", "[graphics][CubeMap]"){
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::PositiveX}, 3) ==  Vec3r::UnitX);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::NegativeX}, 3) == -Vec3r::UnitX);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::PositiveY}, 3) ==  Vec3r::UnitY);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::NegativeY}, 3) == -Vec3r::UnitY);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::PositiveZ}, 3) ==  Vec3r::UnitZ);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::NegativeZ}, 3) == -Vec3r::UnitZ);

	CHECK(xen::getCubeMapDirection({2,1,xen::CubeMap::Face::PositiveZ}, 3) ==
	      xen::normalized(Vec3r{-0.666666_r, 0, 1.0_r}));
	CHECK(xen::getCubeMapDirection({2,2,xen::CubeMap::Face::PositiveZ}, 3) ==
	      xen::normalized(Vec3r{-0.666666_r, 0.666666_r, 1.0_r}));
}

TEST_CASE("Cube Map LatLong to Pixel", "[graphics][CubeMap][latlong]"){
	// Latitude test
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 90_deg, 0_deg }, 3) ==
		Vec3u { 1, 1, xen::CubeMap::Face::PositiveY }
	);
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ -90_deg, 0_deg }, 3) ==
		Vec3u { 1, 1, xen::CubeMap::Face::NegativeY }
	);

	// Rotation around equator
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 0_deg, 0_deg }, 3) ==
		Vec3u { 1, 1, xen::CubeMap::Face::PositiveX }
	);
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 0_deg, 90_deg }, 3) ==
		Vec3u { 1, 1, xen::CubeMap::Face::NegativeZ }
	);
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 0_deg, 180_deg }, 3) ==
		Vec3u { 1, 1, xen::CubeMap::Face::NegativeX }
	);
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 0_deg, 270_deg }, 3) ==
		Vec3u { 1, 1, xen::CubeMap::Face::PositiveZ }
	);

	// Intermediate rotations
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 0_deg, 30_deg }, 3) ==
		Vec3u { 0, 1, xen::CubeMap::Face::PositiveX }
	);
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 0_deg, 60_deg }, 3) ==
		Vec3u { 2, 1, xen::CubeMap::Face::NegativeZ }
	);
	CHECK(
		xen::getCubeMapPixelCoord( xen::LatLong{ 30_deg, 0_deg }, 3) ==
		Vec3u { 1, 2, xen::CubeMap::Face::PositiveX }
	);
}

TEST_CASE("Cube Map Pixel to LatLong (size 3)", "[graphics][CubeMap][latlong]"){
	// Rotation around equator
	CHECK(
		xen::getCubeMapLatLong(Vec3u{ 1, 1, xen::CubeMap::Face::PositiveX }, 3) ==
		xen::LatLong{ 0_deg, 0_deg }
	);
	CHECK(
		xen::getCubeMapLatLong(Vec3u{ 1, 1, xen::CubeMap::Face::NegativeZ }, 3) ==
		xen::LatLong{ 0_deg, 90_deg }
	);
	CHECK(
		xen::getCubeMapLatLong(Vec3u{ 1, 1, xen::CubeMap::Face::NegativeX }, 3) ==
		xen::LatLong{ 0_deg, 180_deg }
	);
	CHECK(
		xen::getCubeMapLatLong(Vec3u{ 1, 1, xen::CubeMap::Face::PositiveZ }, 3) ==
		xen::LatLong{ 0_deg, -90_deg }
	);

	// Poles
	CHECK(
		xen::getCubeMapLatLong(Vec3u{ 1, 1, xen::CubeMap::Face::PositiveY }, 3) ==
		xen::LatLong{  90_deg, 0_deg }
	);
	CHECK(
		xen::getCubeMapLatLong(Vec3u{ 1, 1, xen::CubeMap::Face::NegativeY }, 3) ==
		xen::LatLong{ -90_deg, 0_deg }
	);
}

TEST_CASE("Cube Map UV from Direction", "[graphics][CubeMap]"){
	CHECK(xen::getCubeMapUv(Vec3r{  1,  0,  0 }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX});
	CHECK(xen::getCubeMapUv(Vec3r{ -1,  0,  0 }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeX});

	CHECK(xen::getCubeMapUv(Vec3r{  0,  1,  0 }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveY});
	CHECK(xen::getCubeMapUv(Vec3r{  0, -1,  0 }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeY});

	CHECK(xen::getCubeMapUv(Vec3r{  0,  0,  1 }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveZ});
	CHECK(xen::getCubeMapUv(Vec3r{  0,  0, -1 }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeZ});
}

TEST_CASE("Cube Map Direction from UV", "[graphics][CubeMap]"){
	CHECK(xen::getCubeMapDirection(xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX}) == Vec3r{  1,  0,  0 });
	CHECK(xen::getCubeMapDirection(xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeX}) == Vec3r{ -1,  0,  0 });

	CHECK(xen::getCubeMapDirection(xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveY}) == Vec3r{  0,  1,  0 });
	CHECK(xen::getCubeMapDirection(xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeY}) == Vec3r{  0, -1,  0 });

	CHECK(xen::getCubeMapDirection(xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveZ}) == Vec3r{  0,  0,  1 });
	CHECK(xen::getCubeMapDirection(xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeZ}) == Vec3r{  0,  0, -1 });
}

TEST_CASE("Cube Map Direction -> UV -> Direction"){
	Vec3r dir;
	for(dir.x = -10_r; dir.x <= 10.0; dir.x += 1.5_r){
		for(dir.y = -10_r; dir.y <= 10.0; dir.y += 1.5_r){
			for(dir.z = -10_r; dir.z <= 10.0; dir.z += 1.5_r){
				Vec3r dir_n = xen::normalized(dir);
				CHECK(xen::getCubeMapDirection(xen::getCubeMapUv(dir_n)) == dir_n);
			}
		}
	}
}
