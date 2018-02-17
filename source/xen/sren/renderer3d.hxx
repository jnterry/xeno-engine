////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of rendering functions for software renderer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_HPP
#define XEN_GRAPHICS_SREN_RENDERER3D_HPP

#include <xen/graphics/Color.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/math/geometry_types.hpp>

#include <cstdio>

namespace xen{
	// forward declarations
	struct Camera3d;
	struct RenderCommand3d;

	namespace sren {
		/// \todo
		typedef RawImage RenderTarget;

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a render target to the
		/// specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RenderTarget& target, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a section of a render target
		/// to the specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RenderTarget& target, const xen::Aabb2u& viewport, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands using software rasterizer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param camera The 3d camera used to view the scene
		/// \param commands Array of render commands to perform
		/// \param command_count The number of commands in the comands array
		/////////////////////////////////////////////////////////////////////
		void renderRasterize(RenderTarget& target,
		                     const xen::Aabb2u& viewport,
		                     const Camera3d& camera,
		                     RenderCommand3d* commands, u32 command_count);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands use software raytracer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param camera The 3d camera used to view the scene
		/// \param commands Array of render commands to perform
		/// \param command_count The number of commands in the comands array
		/////////////////////////////////////////////////////////////////////
		void renderRaytrace (RenderTarget& target,
		                     const xen::Aabb2u& viewport,
		                     const Camera3d& camera,
		                     RenderCommand3d* commands, u32 command_count);

	}

}

#endif
