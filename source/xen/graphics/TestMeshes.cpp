////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declerations of various test meshes of use for rendering
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TESTMESHES_CPP
#define XEN_GRAPHICS_TESTMESHES_CPP

#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/TestMeshes.hpp>

namespace {

	//////////////////////////////////////////////////////////////////////////////

	Vec3r UnitCubeLines_Positions[] = {
		// Front face, z = 0
		{ 0_r,  0_r,  0_r }, { 1_r,  0_r,  0_r },
		{ 1_r,  0_r,  0_r }, { 1_r,  1_r,  0_r },
		{ 1_r,  1_r,  0_r }, { 0_r,  1_r,  0_r },
		{ 0_r,  1_r,  0_r }, { 0_r,  0_r,  0_r },

		// Back face, z = 1
		{ 0_r,  0_r,  1_r }, { 1_r,  0_r,  1_r },
		{ 1_r,  0_r,  1_r }, { 1_r,  1_r,  1_r },
		{ 1_r,  1_r,  1_r }, { 0_r,  1_r,  1_r },
		{ 0_r,  1_r,  1_r }, { 0_r,  0_r,  1_r },

		// Connecting lines
		{ 1_r,  1_r,  1_r }, { 1_r,  1_r,  0_r },
		{ 0_r,  1_r,  1_r }, { 0_r,  1_r,  0_r },
		{ 0_r,  0_r,  1_r }, { 0_r,  0_r,  0_r },
		{ 1_r,  0_r,  1_r }, { 1_r,  0_r,  0_r },
	};

	xen::Color UnitCubeLines_Colors[] = {
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,

		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,

		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE,
	};

	static_assert(XenArrayLength(UnitCubeLines_Positions) == XenArrayLength(UnitCubeLines_Colors),
	              "Expected same number of colors and positions"
	              );

	//////////////////////////////////////////////////////////////////////////////

	Vec3r UnitCube_Positions[] = {
		// Front Face, z = 0
		{ 0_r, 0_r, 0_r }, { 0_r, 1_r, 0_r }, { 1_r, 0_r, 0_r },
		{ 1_r, 0_r, 0_r }, { 0_r, 1_r, 0_r }, { 1_r, 1_r, 0_r },

		// Back Face, z = 1
		{ 0_r, 0_r, 1_r }, { 1_r, 0_r, 1_r }, { 0_r, 1_r, 1_r },
		{ 1_r, 0_r, 1_r }, { 1_r, 1_r, 1_r }, { 0_r, 1_r, 1_r },

		// Bottom face, y = 0
	  { 0_r, 0_r, 0_r }, { 1_r, 0_r, 0_r }, { 0_r, 0_r, 1_r },
		{ 1_r, 0_r, 0_r }, { 1_r, 0_r, 1_r }, { 0_r, 0_r, 1_r },

		// Top face, y = 1
		{ 0_r, 1_r, 0_r }, { 0_r, 1_r, 1_r }, { 1_r, 1_r, 0_r },
		{ 1_r, 1_r, 0_r }, { 0_r, 1_r, 1_r }, { 1_r, 1_r, 1_r },

		// Left face, x = 0
		{ 0_r, 0_r, 0_r }, { 0_r, 0_r, 1_r }, { 0_r, 1_r, 0_r },
		{ 0_r, 1_r, 0_r }, { 0_r, 0_r, 1_r }, { 0_r, 1_r, 1_r },

		// Right face, x = 1
		{ 1_r, 0_r, 0_r }, { 1_r, 1_r, 0_r }, { 1_r, 0_r, 1_r },
		{ 1_r, 1_r, 0_r }, { 1_r, 1_r, 1_r }, { 1_r, 0_r, 1_r },
	};

	Vec3r UnitCube_Normals[] = {
		// Front face, z = 0
		Vec3r::UnitZ, Vec3r::UnitZ, Vec3r::UnitZ,
		Vec3r::UnitZ, Vec3r::UnitZ, Vec3r::UnitZ,

		// Back Face, z = 1
		-Vec3r::UnitZ, -Vec3r::UnitZ, -Vec3r::UnitZ,
		-Vec3r::UnitZ, -Vec3r::UnitZ, -Vec3r::UnitZ,

		// Bottom face, y = 0
	  Vec3r::UnitY, Vec3r::UnitY, Vec3r::UnitY,
		Vec3r::UnitY, Vec3r::UnitY, Vec3r::UnitY,

		// Top face, y = 1
	  -Vec3r::UnitY, -Vec3r::UnitY, -Vec3r::UnitY,
		-Vec3r::UnitY, -Vec3r::UnitY, -Vec3r::UnitY,

		// Left face, y = 0
	  Vec3r::UnitX, Vec3r::UnitX, Vec3r::UnitX,
		Vec3r::UnitX, Vec3r::UnitX, Vec3r::UnitX,

		// Right face, y = 1
	  -Vec3r::UnitX, -Vec3r::UnitX, -Vec3r::UnitX,
		-Vec3r::UnitX, -Vec3r::UnitX, -Vec3r::UnitX,
	};

	xen::Color UnitCube_Colors[] = {
		// Front face, z = 0
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,

		// Back Face, z = 1
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,

		// Bottom face, y = 0
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,

		// Top face, y = 1
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,

		// Left face, y = 0
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,

		// Right face, y = 1
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
		xen::Color::WHITE, xen::Color::WHITE, xen::Color::WHITE,
	};

	static_assert(XenArrayLength(UnitCube_Positions) == XenArrayLength(UnitCube_Colors),
	              "Expected same number of colors and positions"
	              );
	static_assert(XenArrayLength(UnitCube_Positions) == XenArrayLength(UnitCube_Normals),
	              "Expected same number of normals and positions"
	              );

	//////////////////////////////////////////////////////////////////////////////

	Vec3r Axes_Positions[] = {
		Vec3r::Origin, Vec3r::UnitX,
		Vec3r::Origin, Vec3r::UnitY,
		Vec3r::Origin, Vec3r::UnitZ,
	};

	xen::Color Axes_Colors[] = {
		xen::Color::RED,   xen::Color::RED,
		xen::Color::GREEN, xen::Color::GREEN,
		xen::Color::BLUE,  xen::Color::BLUE,
	};

	static_assert(XenArrayLength(Axes_Positions) == XenArrayLength(Axes_Colors),
	              "Expected same number of colors and positions"
	             );

	//////////////////////////////////////////////////////////////////////////////

}

namespace xen {
	const MeshGeometrySource TestMeshGeometry_UnitCubeLines = {
		XenArrayLength(UnitCubeLines_Positions),
		UnitCubeLines_Positions,
		nullptr,
		UnitCubeLines_Colors,
	};

	const MeshGeometrySource TestMeshGeometry_UnitCube = {
		XenArrayLength(UnitCube_Positions),
		UnitCube_Positions,
		UnitCube_Normals,
		UnitCube_Colors,
	};

	const MeshGeometrySource TestMeshGeometry_Axes = {
		XenArrayLength(Axes_Positions),
		Axes_Positions,
		nullptr,
		Axes_Colors,
	};
}

#endif
