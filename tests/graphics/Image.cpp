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

TEST_CASE("Cube Map Direction -> UV -> Direction", "[graphics][CubeMap]"){
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

TEST_CASE("LatLong -> CubeMap UV", "[graphics][CubeMap]"){
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg,   0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg,  90_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeZ });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, 180_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, 270_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveZ });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, 360_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });

	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, -  0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, - 90_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveZ });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, -180_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, -270_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeZ });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 0_deg, -360_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });

	CHECK(xen::getCubeMapUv(xen::LatLong{   0_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });
	CHECK(xen::getCubeMapUv(xen::LatLong{  90_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveY });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 180_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 270_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeY });
	CHECK(xen::getCubeMapUv(xen::LatLong{ 360_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });

	CHECK(xen::getCubeMapUv(xen::LatLong{ -  0_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ - 90_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeY });
	CHECK(xen::getCubeMapUv(xen::LatLong{ -180_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::NegativeX });
	CHECK(xen::getCubeMapUv(xen::LatLong{ -270_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveY });
	CHECK(xen::getCubeMapUv(xen::LatLong{ -360_deg, 0_deg }) == xen::CubeMapUv{ 0.5_r, 0.5_r, xen::CubeMap::PositiveX });

	CHECK(xen::getCubeMapUv(xen::LatLong{ 0.0000_deg,  45_deg }) == xen::CubeMapUv{ 1.0_r, 0.5_r, xen::CubeMap::NegativeZ });
	CHECK(xen::getCubeMapUv(xen::LatLong{35.2644_deg, -45_deg }) == xen::CubeMapUv{ 0.0_r, 0.0_r, xen::CubeMap::PositiveY });
}


TEST_CASE("CubeMap UV -> LatLong", "[graphics][CubeMap]"){
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 1.0_r, 1.0_r, xen::CubeMap::PositiveY }) == xen::LatLong{  35.2644_deg,  135_deg });
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 0.0_r, 0.0_r, xen::CubeMap::PositiveY }) == xen::LatLong{  35.2644_deg, - 45_deg });
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 1.0_r, 0.0_r, xen::CubeMap::PositiveY }) == xen::LatLong{  35.2644_deg, -135_deg });
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 0.0_r, 1.0_r, xen::CubeMap::PositiveY }) == xen::LatLong{  35.2644_deg,   45_deg });

	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 1.0_r, 1.0_r, xen::CubeMap::NegativeY }) == xen::LatLong{ -35.2644_deg, -135_deg });
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 0.0_r, 0.0_r, xen::CubeMap::NegativeY }) == xen::LatLong{ -35.2644_deg,   45_deg });
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 1.0_r, 0.0_r, xen::CubeMap::NegativeY }) == xen::LatLong{ -35.2644_deg,  135_deg });
	CHECK(xen::getCubeMapLatLong(xen::CubeMapUv{ 0.0_r, 1.0_r, xen::CubeMap::NegativeY }) == xen::LatLong{ -35.2644_deg, - 45_deg });
}

TEST_CASE("Map LatLong -> CubeMap UV -> LatLong", "[graphics][CubeMap]"){
	xen::LatLong ll;

	// Avoid singularities with non-unique representations (eg, longitude
	// irrelevant if latitude is 90) by starting and ending at just off 180

	for(ll.x = -179.9_deg; ll.x <= 179.9_deg; ll.x += 10_deg){
		for(ll.y = -179.9_deg; ll.y <= 179.9_deg; ll.y += 10_deg){
			CHECK(xen::clamp(xen::getCubeMapLatLong(xen::getCubeMapUv(ll))) == xen::clamp(ll));
		}
	}
}

TEST_CASE("getCubeMapSamplePoints", "[graphics][CubeMap]"){

	SECTION("Size 3, LatLong 0, 0"){
		xen::LatLong ll = { 0_deg, 0_deg };
		xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(ll, 3);

		CHECK(sp.coord [0] == Vec3u{ 1, 1, xen::CubeMap::PositiveX });
		CHECK(sp.weight[0] == 1.0_r);

		CHECK(sp.weight[1] == 0.0_r);
		CHECK(sp.weight[2] == 0.0_r);
		CHECK(sp.weight[3] == 0.0_r);
	}

	SECTION("Size 9, LatLong 90, 0"){
		xen::LatLong ll = { 90_deg, 0_deg };
		xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(ll, 9);

		CHECK(sp.coord [0] == Vec3u{ 4, 4, xen::CubeMap::PositiveY });
		CHECK(sp.weight[0] == 1.0_r);

		CHECK(sp.weight[1] == 0.0_r);
		CHECK(sp.weight[2] == 0.0_r);
		CHECK(sp.weight[3] == 0.0_r);
	}

	SECTION("Size 2, LatLong 0, 0"){
		xen::LatLong ll = { 0_deg, 0_deg };
		xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(ll, 2);

		CHECK(sp.coord [3] == Vec3u{ 0, 0, xen::CubeMap::PositiveX });
		CHECK(sp.weight[3] == 0.25_r);

		CHECK(sp.coord [2] == Vec3u{ 1, 0, xen::CubeMap::PositiveX });
		CHECK(sp.weight[2] == 0.25_r);

		CHECK(sp.coord [1] == Vec3u{ 0, 1, xen::CubeMap::PositiveX });
		CHECK(sp.weight[1] == 0.25_r);

		CHECK(sp.coord [0] == Vec3u{ 1, 1, xen::CubeMap::PositiveX });
		CHECK(sp.weight[0] == 0.25_r);
	}

	SECTION("Size 3, LatLong 0, 45"){
		xen::LatLong ll = { 0_deg, 44.99999_deg };
		xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(ll, 3);

		CHECK(sp.weight[0] == Approx(0.5_r));
		CHECK(sp.weight[1] == Approx(0.5_r));
		CHECK(sp.weight[2] == Approx(0.0_r));
		CHECK(sp.weight[3] == Approx(0.0_r));

		CHECK(sp.coord[0] == Vec3u{ 0, 1, xen::CubeMap::PositiveX });
		CHECK(sp.coord[1] == Vec3u{ 2, 1, xen::CubeMap::NegativeZ });
	}

	SECTION("Size 1, Dir 1,1,1"){
		xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(xen::normalized(Vec3r{1,0.999999,0.999999}), 1);

		CHECK(sp.weight[0] == Approx(0.3333333333333_r));
		CHECK(sp.weight[1] == Approx(0.3333333333333_r));
		CHECK(sp.weight[2] == Approx(0.3333333333333_r));
		CHECK(sp.weight[3] == Approx(0.0_r));
	}
}

TEST_CASE("getCubeMapLatLong -> SamplePoints", "[graphics][CubeMap]"){
	Vec3u pos;
	for(pos.z = 0; pos.z < 6; ++pos.z){
		for(pos.x = 0; pos.x < 5; ++pos.x){
			for(pos.y = 0; pos.y < 5; ++pos.y){


				xen::LatLong ll = xen::getCubeMapLatLong(pos, 5);
				xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(ll, 5);

				CHECK(sp.weight[0] == Approx(1.0_r).scale(0.05));
				CHECK(sp.weight[1] == Approx(0.0_r).scale(0.05));
				CHECK(sp.weight[2] == Approx(0.0_r).scale(0.05));
				CHECK(sp.weight[3] == Approx(0.0_r).scale(0.05));
				CHECK(sp.coord[0] == pos);
			}
		}
	}
}

TEST_CASE("getCubeMapLatLong -> SamplePoints Regression", "[graphics][CubeMap]"){
	// This is a regression test for a bug found in real use case
	// Function was returning a position just out of bounds (IE: y = 64
	// on a cubemap of 64x64)
	// Original output:
	// 8, 64, 0 @@ 0.272599
	// 9, 64, 0 @@ 0.227401
	// 8, 63, 0 @@ 0.272599
	// 9, 63, 0 @@ 0.227401
	xen::LatLong ll = { xen::Degrees(39.058353), xen::Degrees(35.760014) };
	xen::CubeMapSamplePoints sp = xen::getCubeMapSamplePoints(ll, 64);

	for(int i = 0; i < 4; ++i){
		printf("%u, %u, %u @@ %f\n",
		       sp.coord[i].x,
		       sp.coord[i].y,
		       sp.coord[i].z,
		       sp.weight[i]);
	}
}
