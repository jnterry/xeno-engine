////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen camera types and related functions
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/graphics/Image.hpp>
#include "../math/ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Cube Map Direction to Pixel", "[graphics][CubeMap][]"){
	CHECK(xen::getCubeMapPixelCoord( Vec3r::UnitX, { 3, 3 }) == Vec3u { 1, 1, xen::CubeMap::Face::PositiveX });
	CHECK(xen::getCubeMapPixelCoord(-Vec3r::UnitX, { 3, 3 }) == Vec3u { 1, 1, xen::CubeMap::Face::NegativeX });
	CHECK(xen::getCubeMapPixelCoord( Vec3r::UnitY, { 3, 3 }) == Vec3u { 1, 1, xen::CubeMap::Face::PositiveY });
	CHECK(xen::getCubeMapPixelCoord(-Vec3r::UnitY, { 3, 3 }) == Vec3u { 1, 1, xen::CubeMap::Face::NegativeY });
	CHECK(xen::getCubeMapPixelCoord( Vec3r::UnitZ, { 3, 3 }) == Vec3u { 1, 1, xen::CubeMap::Face::PositiveZ });
	CHECK(xen::getCubeMapPixelCoord(-Vec3r::UnitZ, { 3, 3 }) == Vec3u { 1, 1, xen::CubeMap::Face::NegativeZ });

	CHECK(xen::getCubeMapPixelCoord({ 0.0, 0.8, 1 }, { 3, 3 }) ==
	      Vec3u { 1, 2, xen::CubeMap::Face::PositiveZ });
	CHECK(xen::getCubeMapPixelCoord({ 0.0, -0.8, 1 }, { 3, 3 }) ==
	      Vec3u { 1, 0, xen::CubeMap::Face::PositiveZ });
	CHECK(xen::getCubeMapPixelCoord({ 0.8, 0.8, 1 }, { 3, 3 }) ==
	      Vec3u { 2, 2, xen::CubeMap::Face::PositiveZ });

	CHECK(xen::getCubeMapPixelCoord({ 0.0, 0.8, -1 }, { 3, 3 }) ==
	      Vec3u { 1, 2, xen::CubeMap::Face::NegativeZ });
	CHECK(xen::getCubeMapPixelCoord({ 0.0, -0.8, -1 }, { 3, 3 }) ==
	      Vec3u { 1, 0, xen::CubeMap::Face::NegativeZ });
}

TEST_CASE("Cube Map Pixel to Direction", "[graphics][CubeMap][]"){
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::PositiveX}, {3,3}) ==  Vec3r::UnitX);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::NegativeX}, {3,3}) == -Vec3r::UnitX);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::PositiveY}, {3,3}) ==  Vec3r::UnitY);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::NegativeY}, {3,3}) == -Vec3r::UnitY);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::PositiveZ}, {3,3}) ==  Vec3r::UnitZ);
	CHECK(xen::getCubeMapDirection({1,1,xen::CubeMap::Face::NegativeZ}, {3,3}) == -Vec3r::UnitZ);

	CHECK(xen::getCubeMapDirection({2,1,xen::CubeMap::Face::PositiveZ}, {3,3}) ==
	      xen::normalized(Vec3r{0.666666_r, 0, 1.0_r}));
	CHECK(xen::getCubeMapDirection({2,2,xen::CubeMap::Face::PositiveZ}, {3,3}) ==
	      xen::normalized(Vec3r{0.666666_r, 0.666666_r, 1.0_r}));
}
