////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declerations of various test meshes of use for rendering
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TESTMESHES_HPP
#define XEN_GRAPHICS_TESTMESHES_HPP

#include <xen/graphics/RenderCommand3d.hpp>

namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Geometry for a test mesh that represents the unit cube,
	/// IE, [0,0,0] -> [1,1,1]
	///
	/// This geometry is laid out in such a way it may be drawn using the
	/// LINES PrimitiveType
	/////////////////////////////////////////////////////////////////////
	const extern MeshGeometrySource TestMeshGeometry_UnitCubeLines;

	/////////////////////////////////////////////////////////////////////
	/// \brief Geometry for a test mesh that represents the unit cube,
	/// IE, [0,0,0] -> [1,1,1]
	///
	/// This geometry is laid out in such a way it may be drawn using the
	/// TRIANGLES PrimitiveType
	/////////////////////////////////////////////////////////////////////
	const extern MeshGeometrySource TestMeshGeometry_UnitCube;

	/////////////////////////////////////////////////////////////////////
	/// \brief 2 triangles forming a square of side length 1. Center of
	/// square is at (0,0,0). Square lies flat in the xz plane. Normals
	/// are defined to be in positive y direction
	/////////////////////////////////////////////////////////////////////
	const extern MeshGeometrySource TestMeshGeometry_UnitXzPlaneCentered;

	/////////////////////////////////////////////////////////////////////
	/// \brief Geometry for a test mesh that represents the 3 axes as a line
	/// each of unit length, starting at the origin and extending out in the
	/// positive direction.
	///
	/// X will be drawn in red, Y in green and Z in blue
	///
	/// This geometry is laid out in such a way it may be drawn using the
	/// LINES PrimitiveType
	/////////////////////////////////////////////////////////////////////
	const extern MeshGeometrySource TestMeshGeometry_Axes;
}

#endif
