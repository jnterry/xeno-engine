////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of rendering functions for software renderer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_HPP
#define XEN_GRAPHICS_SREN_RENDERER3D_HPP

#include <xen/core/array_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/math/geometry_types.hpp>

#include <cstdio>

namespace xen{
	// forward declarations
	struct Camera3d;
	struct RenderCommand3d;

	namespace sren {

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a render target to the
		/// specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RawImage& target, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a section of a render target
		/// to the specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RawImage& target, const xen::Aabb2u& viewport, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands using software rasterizer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param params The 3d camera and other rendering parameters used to
		/// view the scene
		/// \param commands Array of render commands to perform
		/////////////////////////////////////////////////////////////////////
		void renderRasterize(RawImage& target,
		                     const xen::Aabb2u& viewport,
		                     const RenderParameters3d& params,
		                     const xen::Array<RenderCommand3d>& commands);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands use software raytracer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param camera The 3d camera used to view the scene
		/// \param commands Array of render commands to perform
		/////////////////////////////////////////////////////////////////////
		void renderRaytrace (RawImage& target,
		                     const xen::Aabb2u& viewport,
		                     const RenderParameters3d& params,
		                     const xen::Array<RenderCommand3d>& commands);

		/////////////////////////////////////////////////////////////////////
		/// \brief Draws debug view of a camera (eg, origin, up dir, look dir, etc),
		/// from some other camera's perspective
		/// \param target      The render target to draw to
		/// \param viewport    The area of the target to draw to
		/// \param view_camera The camera to use as the perspective to draw from
		/// \param camera      The camera to draw
		/////////////////////////////////////////////////////////////////////
		void renderCameraDebug(RawImage& target, const xen::Aabb2u& viewport,
		                       const Camera3d& view_camera,
		                       const Camera3d& camera);

	}

}

#endif
