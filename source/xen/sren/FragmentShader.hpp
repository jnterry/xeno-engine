////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of FragmentShader type and related functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

// :TODO: actual fragment shader type is in RenderCommands3d since we need a
// pointer to it in the RenderCommand. At some point we should make shaders
// an opaque type and have the graphics device create them from something...

#ifndef XEN_SREN_FRAGMENTSHADER_HPP
#define XEN_SREN_FRAGMENTSHADER_HPP

#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

namespace xen {
	namespace sren {

		struct TextureImpl {
			// :TODO: mipmaps
			// :TODO: texture wrapping/clamping/etc settings
			xen::RawImage image;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Samples a texture returning the color of the specified uv
		/// coordinate
		/////////////////////////////////////////////////////////////////////
		Color4f sampleTexture(const TextureImpl&, Vec2r uv);

		/////////////////////////////////////////////////////////////////////
		/// \brief Computes the influence of a light of a particular color at
		/// some distance from it
		///
		/// \param light_pos The position of the light source
		///
		/// \param light_color The color of the light. w component is interpreted
		/// as a modifier for brightness of the light where 1 means full brightness
		/// and 0 means no light emitted.
		///
		/// \param attenuation - Vector representing the attenuation coefficients
		/// of the light where x is some constant, y is a linear coefficient and
		/// z is the quadratic coefficient
		///
		/// \param distance_sq The square of the distance between the light source
		/// and the point it is illuminating
		///
		/// \param eye_pos The position of the virtual camera
		///
		/// \param pos_world The position of the surface being illuminated
		///
		/// \param normal_world The normal to the surface being illuminated
		/////////////////////////////////////////////////////////////////////
		xen::Color3f computeLightInfluencePhong(Vec3r        light_pos,
		                                        xen::Color4f light_color,
		                                        Vec3f        attenuation,
		                                        real         distance_sq,
		                                        Vec3r        eye_pos,
		                                        Vec3r        pos_world,
		                                        Vec3r        normal_world);

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
		xen::Color3f computeLightInfluenceSimple(xen::Color4f light_color,
		                                         Vec3f        attenuation,
		                                         real         distance_sq);


		/// \brief The default fragment shader for use by the software rasterizer
		/// Performs very basic lighting
		extern FragmentShader DefaultFragmentShader;

	}
}

#endif
