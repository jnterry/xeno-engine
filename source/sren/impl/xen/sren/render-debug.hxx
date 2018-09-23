////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of functions for drawing debugging visualisations
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERDEBUG_HPP
#define XEN_SREN_RENDERDEBUG_HPP

#include <xen/sren/rasterizer3d.hxx>

#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array_types.hpp>

#include <cstdio>

namespace xen{
	struct Camera3d;
	struct RenderCommand3d;
}

namespace xsr {

	/////////////////////////////////////////////////////////////////////
	/// \brief Draws debug view of a camera (eg, origin, up dir, look dir, etc),
	/// from some other camera's perspective
	/// \param target      The render target to draw to
	/// \param viewport    The area of the target to draw to
	/// \param view_camera The camera to use as the perspective to draw from
	/// \param camera      The camera to draw
	/// \param scale       The scale of the camera to draw in world units
	/////////////////////////////////////////////////////////////////////
	void renderCameraDebug(xsr::RenderTarget& target,
	                       const xen::Aabb2u&   viewport,
	                       const xen::Camera3d& view_camera,
	                       const xen::Camera3d& camera,
	                       real                 scale);

	/////////////////////////////////////////////////////////////////////
	/// \brief Renders a bounding box to the screen
	/////////////////////////////////////////////////////////////////////
	void renderDebugBoundingBox(xsr::RenderTarget& target,
	                            const xen::Aabb2u&   viewport,
	                            const xen::Camera3d& camera,
	                            xen::Aabb3r          aabb,
	                            xen::Color4f         color = xen::Color::RED4f);

}

#endif
