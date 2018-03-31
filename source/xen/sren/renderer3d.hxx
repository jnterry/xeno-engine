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
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/utilities.hpp>
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

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands use software raytracer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param camera The 3d camera used to view the scene
		/// \param commands Array of render commands to perform
		/////////////////////////////////////////////////////////////////////
		void renderRaytrace (xen::sren::RenderTargetImpl&       target,
		                     const xen::Aabb2u&                 viewport,
		                     const xen::RenderParameters3d&     params,
		                     const xen::Array<RenderCommand3d>& commands);

		/////////////////////////////////////////////////////////////////////
		/// \brief Draws debug view of a camera (eg, origin, up dir, look dir, etc),
		/// from some other camera's perspective
		/// \param target      The render target to draw to
		/// \param viewport    The area of the target to draw to
		/// \param view_camera The camera to use as the perspective to draw from
		/// \param camera      The camera to draw
		/////////////////////////////////////////////////////////////////////
		void renderCameraDebug(xen::sren::RenderTargetImpl& target,
		                       const xen::Aabb2u&           viewport,
		                       const Camera3d&              view_camera,
		                       const Camera3d&              camera);

		/////////////////////////////////////////////////////////////////////
		/// \brief Computes the influence of a light of a particular color at
		/// some distance from it
		/// \param light_color The color of the light. w component is interpreted
		/// as a modifier for brightness of the light where 1 means full brightness
		/// and 0 means no light emitted.
		/// \param attenuation - Vector representing the attenuation coefficients
		/// of the light where x is some constant, y is a linear coefficient and
		/// z is the quadratic coefficient
		/// \param distance_sq The square of the distance between the light source
		/// and the point it is illuminating
		/////////////////////////////////////////////////////////////////////
		xen::Color3f computeLightInfluence(xen::Color4f light_color,
		                                   Vec3f        attenuation,
		                                   real         distance_sq);

	}

}

#endif
