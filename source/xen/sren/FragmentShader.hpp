////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of FragmentShader type and related functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_FRAGMENTSHADER_HPP
#define XEN_SREN_FRAGMENTSHADER_HPP

#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include "Texture.hxx"

namespace xen {
	namespace sren {

		/// \brief Bundle of extra parameters needed for the fragment shader
		struct FragmentUniforms : public RenderParameters3d {
			/// \brief The emissive color of the geometry
			xen::Color4f emissive_color;

			/// \brief The diffuse color to use for geometry
			xen::Color4f diffuse_color;

			/// \brief The model matrix
			Mat4r m_matrix;

			/// \brief The view projection matrix
			Mat4r vp_matrix;

			TextureImpl* textures[4];
		};

		typedef Color4f (*FragmentShader)(const xen::sren::FragmentUniforms& uniforms,
		                                  Vec3r                        pos_world,
		                                  Vec3r                        normal_world,
		                                  xen::Color4f                 color,
		                                  Vec2f                        uvs);

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
