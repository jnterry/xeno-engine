////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of rasterization types and functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZER3D_HXX
#define XEN_SREN_RASTERIZER3D_HXX

#include <xen/math/geometry_types.hpp>

namespace xen {
	struct RenderParameters3d;

	namespace sren {
		struct RenderTargetImpl;

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands using software rasterizer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param params The 3d camera and other rendering parameters used to
		/// view the scene
		/// \param commands Array of render commands to perform
		/////////////////////////////////////////////////////////////////////
		void renderRasterize(xen::sren::RenderTargetImpl&       target,
		                     const xen::Aabb2u&                 viewport,
		                     const xen::RenderParameters3d&     params,
		                     const xen::Array<RenderCommand3d>& commands);

	}
}

#endif
