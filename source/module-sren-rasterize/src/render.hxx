////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of functions for doing rendering with software
/// rasterizer
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_RENDER_HXX
#define XEN_MODULESRENRASTERIZE_RENDER_HXX

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/core/array_types.hpp>

namespace xsr {
	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	           );
}

#endif
