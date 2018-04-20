////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of rendering functions for software renderer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERUTILITIES_HPP
#define XEN_GRAPHICS_SREN_RENDERUTILITIES_HPP

#include "rasterizer3d.hxx"

#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array_types.hpp>

#include <cstdio>

namespace xen{
	// forward declarations
	struct Camera3d;
	struct RenderCommand3d;

	namespace sren {
		struct RenderTargetImpl;

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a render target to the
		/// specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RenderTargetImpl& target, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Clears the diffuse component of a section of a render target
		/// to the specified color
		/////////////////////////////////////////////////////////////////////
		void clear(RenderTargetImpl& target, const xen::Aabb2u& viewport, Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Draws debug view of a camera (eg, origin, up dir, look dir, etc),
		/// from some other camera's perspective
		/// \param target      The render target to draw to
		/// \param viewport    The area of the target to draw to
		/// \param view_camera The camera to use as the perspective to draw from
		/// \param camera      The camera to draw
		/// \param scale       The scale of the camera to draw in world units
		/////////////////////////////////////////////////////////////////////
		void renderCameraDebug(xen::sren::RenderTargetImpl& target,
		                       const xen::Aabb2u&           viewport,
		                       const Camera3d&              view_camera,
		                       const Camera3d&              camera,
		                       real                         scale);

		/////////////////////////////////////////////////////////////////////
		/// \brief Renders a bounding box to the screen
		/////////////////////////////////////////////////////////////////////
		void renderDebugBoundingBox(RasterizationContext context,
		                            xen::Aabb3r          aabb,
		                            xen::Color4f         color = xen::Color::RED4f
		                           );

	}

}

#endif
