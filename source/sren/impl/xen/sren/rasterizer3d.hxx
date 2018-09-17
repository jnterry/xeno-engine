////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of rasterization types and functions
///
/// \todo :TODO: we would prefer this file to be in module-sren-rasterize
/// however it is required by render-debug.hxx which contains utilities
/// used by all sren backends
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZER3D_HXX
#define XEN_SREN_RASTERIZER3D_HXX

#include <xen/sren/FragmentShader.hpp>
#include <xen/graphics/Mesh_types.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/vertex_group_types.hpp>

namespace xen {
	struct RenderParameters3d;
}

namespace xsr {
	struct RenderTarget;

	struct RasterizerMesh : public xen::MeshHeader, xen::MeshAttribArrays {
		// Anything else?
		// - can we sort the geometry to make rendering more efficient somehow?
		// - anything we can pre-compute?
	};

	/// \brief Bundle of all meta data required for rasterization operations
	struct RasterizationContext : public xsr::FragmentUniforms {
		xsr::RenderTarget*  target;
		xen::Aabb2r*          viewport;
		xsr::FragmentShader fragment_shader;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Rasterizes a set of points defined in model space to the screen
	/// \param context      The context within which the rasterization should occur
	/// \param pos_model    The positions of the points to render in model space
	///                     Array length should equal vertex_count
	/// \param color_buffer Array of per vertex colors. May be nullptr in which
	///                     case the base_color will be used unmodified
	/// \param vertex_count Number of vertices to draw
	/////////////////////////////////////////////////////////////////////
	void rasterizePointsModel(const RasterizationContext& context,
	                          const Vec3r*                pos_model,
	                          const xen::Color*           color_buffer,
	                          const u32                   vertex_count);

	/////////////////////////////////////////////////////////////////////
	/// \brief Rasterizes a single line defined in model space to the screen
	/////////////////////////////////////////////////////////////////////
	void rasterizeLineModel(const RasterizationContext& context,
	                        const xen::LineSegment3r&   line_model,
	                        const xen::LineSegment4f&   line_color);

	/////////////////////////////////////////////////////////////////////
	/// \brief Rasterizes a set of lines in model space
	/// \param vertex_count The number of vertices to draw, length of
	/// pos_model and color_buffer arrays must be at least this long
	/// \param stride The offset between the start vertex of each line
	/// Set to 1 to replicate OpenGL's LINE_STRIP, and 2 to replicate LINES
	/////////////////////////////////////////////////////////////////////
	void rasterizeLinesModel(const RasterizationContext& context,
	                         const Vec3r*                pos_model,
	                         const xen::Color*           color_buffer,
	                         const u32                   vertex_count,
	                         const u32                   stride = 2);


	/////////////////////////////////////////////////////////////////////
	/// \brief Rasterizes a single triangle defined in model space to the screen
	/////////////////////////////////////////////////////////////////////
	void rasterizeTriangleModel(const RasterizationContext& context,
	                            xen::Triangle3r             tri_model,
	                            xen::Triangle3r             tri_normal_model,
	                            xen::Triangle4f             tri_color,
	                            xen::Triangle2f             tri_uvs);

	/////////////////////////////////////////////////////////////////////
	/// \brief Rasterizes a set of triangles in model space to the screen
	/// \param normal_model Vertex normals in model space. May be nullptr
	/// \param color_buffer Per vertex colors, may be nullptr
	/////////////////////////////////////////////////////////////////////
	void rasterizeTrianglesModel(const RasterizationContext& context,
	                             const Vec3r*                pos_model,
	                             const Vec3r*                normal_model,
	                             const xen::Color*           color_buffer,
	                             const Vec2f*                uv_buffer,
	                             const u32                   vertex_count);

	/////////////////////////////////////////////////////////////////////
	/// \brief Rasterizes the specified mesh in the specified context
	/////////////////////////////////////////////////////////////////////
	void rasterizeMesh(const RasterizationContext&   context,
	                   xen::PrimitiveType            primitive_type,
	                   const xen::MeshAttribArrays&  mesh);
}

#endif
