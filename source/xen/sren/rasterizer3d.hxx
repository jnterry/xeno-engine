////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of rasterization types and functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZER3D_HXX
#define XEN_SREN_RASTERIZER3D_HXX

#include <xen/graphics/Mesh_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/vertex_group_types.hpp>

namespace xen {
	struct RenderParameters3d;

	namespace sren {
		struct RenderTargetImpl;

		struct RasterizerMesh : public MeshGeometrySource {
			// nothing here yet...
			//
			// - Bounding box to cull off screen meshes?
			// - can we sort the geometry to make rendering more efficient somehow?
			// - anything we can pre-compute?
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Rasterizes a set of points defined in model space to the screen
		/// \param target       The render target to draw on
		/// \param params       Render parameters describing the scene as a whole
		/// \param viewport     The viewport of the target that may be drawn to
		/// \param m_matrix     Model matrix representing transform from model space
		///                     to world space
		/// \param vp_matrix    View-projection matrix representing transform from
		///                     world space to clip space
		/// \param base_color   The base color of points
		/// \param pos_model    The positions of the points to render in model space
		///                     Array length should equal vertex_count
		/// \param color_buffer Array of per vertex colors. May be nullptr in which
		///                     case the base_color will be used unmodified
		/// \param vertex_count Number of vertices to draw
		/////////////////////////////////////////////////////////////////////
		void rasterizePointsModel(xen::sren::RenderTargetImpl&        target,
		                          const xen::Aabb2r&                  viewport,
		                          const xen::RenderParameters3d&      params,
		                          const Mat4r&                        m_matrix,
		                          const Mat4r&                        vp_matrix,
		                          const xen::Color4f                  color,
		                          const Vec3r*                        pos_model,
		                          const xen::Color*                   color_buffer,
		                          const u32                           vertex_count);

		/////////////////////////////////////////////////////////////////////
		/// \brief Rasterizes a single line defined in model space to the screen
		/////////////////////////////////////////////////////////////////////
		void rasterizeLineModel(xen::sren::RenderTargetImpl&  target,
		                        const xen::Aabb2r&            viewport,
		                        const xen::RenderParameters3d params,
		                        const Mat4r&                  m_matrix,
		                        const Mat4r&                  vp_matrix,
		                        const xen::LineSegment3r&     line_model,
		                        const xen::LineSegment4f&     line_color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Rasterizes a set of lines in model space
		/// \param vertex_count The number of vertices to draw, length of
		/// pos_model and color_buffer arrays must be at least this long
		/// \param stride The offset between the start vertex of each line
		/// Set to 1 to replicate OpenGL's LINE_STRIP, and 2 to replicate LINES
		/////////////////////////////////////////////////////////////////////
		void rasterizeLinesModel(xen::sren::RenderTargetImpl&  target,
		                         const xen::Aabb2r&            viewport,
		                         const xen::RenderParameters3d params,
		                         const Mat4r&                  m_matrix,
		                         const Mat4r&                  vp_matrix,
		                         const xen::Color4f            color,
		                         const Vec3r*                  pos_model,
		                         const xen::Color*             color_buffer,
		                         const u32                     vertex_count,
		                         const u32                     stride = 2);


		/////////////////////////////////////////////////////////////////////
		/// \brief Rasterizes a single triangle defined in model space to the screen
		/////////////////////////////////////////////////////////////////////
		void rasterizeTriangleModel(xen::sren::RenderTargetImpl&  target,
		                            const xen::Aabb2r&            viewport,
		                            const xen::RenderParameters3d params,
		                            const Mat4r&                  m_matrix,
		                            const Mat4r&                  vp_matrix,
		                            const xen::Triangle3r&        tri_model,
		                            const xen::Triangle3r&        tri_normal_model,
		                            xen::Triangle4f               tri_color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Rasterizes a set of triangles in model space to the screen
		/// \param normal_model Vertex normals in model space. May be nullptr
		/// \param color_buffer Per vertex colors, may be nullptr
		/////////////////////////////////////////////////////////////////////
		void rasterizeTrianglesModel(xen::sren::RenderTargetImpl&  target,
		                             const xen::Aabb2r&            viewport,
		                             const xen::RenderParameters3d params,
		                             const Mat4r&                  m_matrix,
		                             const Mat4r&                  vp_matrix,
		                             const xen::Color4f            base_color,
		                             const Vec3r*                  pos_model,
		                             const Vec3r*                  normal_model,
		                             const xen::Color*             color_buffer,
		                             const u32                     vertex_count);
	}
}

#endif
