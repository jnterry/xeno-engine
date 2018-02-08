////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of rendering functions for software renderer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_HPP
#define XEN_GRAPHICS_SREN_RENDERER3D_HPP

#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/graphics/Camera3d.hpp>

#include <cstdio>

namespace xen{
	namespace sren {
		/// \todo
		typedef RawImage RenderTarget;

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a render target to the
		/// specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RenderTarget& target, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands using software rasterizer
		/////////////////////////////////////////////////////////////////////
		void renderRasterize(RenderTarget& target, const Camera3d& camera, RenderCommand3d* commands, u32 command_count);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands use software raytracer
		/////////////////////////////////////////////////////////////////////
		void renderRaytrace (RenderTarget& target, const Camera3d& camera, RenderCommand3d* commands, u32 command_count);

	}

}

#endif
