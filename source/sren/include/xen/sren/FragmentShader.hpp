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
#include <xen/sren/Texture.hpp>

namespace xen {
  struct MaterialParams;
}

namespace xsr {

	/// \brief Bundle of extra parameters needed for the fragment shader
	struct FragmentUniforms : public xen::RenderParameters3d {
		/// \brief The emissive color of the geometry
		xen::Color4f emissive_color;

		/// \brief The diffuse color to use for geometry
		xen::Color4f diffuse_color;

		/// \brief The specular exponent of the material
		real specular_exponent;

		/// \brief Multiplier affecting specular intensity
		real specular_intensity;

		/// \brief The model matrix
		Mat4r m_matrix;

		/// \brief The view projection matrix
		Mat4r vp_matrix;

		xsr::Texture* textures[4];
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Sets the fragment uniforms which change per render command
	/// based on specified parameters
	/////////////////////////////////////////////////////////////////////
	void setPerCommandFragmentUniforms(FragmentUniforms& uniforms,
	                                   const xen::MaterialParams&   material,
	                                   const Mat4r&      m_mat,
	                                   const Mat4r&      vp_mat
	                                  );

	typedef xen::Color4f (*FragmentShader)(const xsr::FragmentUniforms& uniforms,
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
	///
	/// \param specular_exponent The specular exponent to use, larger values
	/// decrease the size of specular highlights
	/////////////////////////////////////////////////////////////////////
	xen::Color3f computeLightInfluencePhong(Vec3r        light_pos,
	                                        xen::Color4f light_color,
	                                        Vec3f        attenuation,
	                                        real         distance_sq,
	                                        Vec3r        eye_pos,
	                                        Vec3r        pos_world,
	                                        Vec3r        normal_world,
	                                        real         specular_exponent,
	                                        real         specular_intensity);

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
	extern FragmentShader FragmentShader_Default;

	/// \brief Fragment shader which just renders everything as white
	extern FragmentShader FragmentShader_AllWhite;

	/// \brief Fragment shader which just renderes everything as its diffuse color
	extern FragmentShader FragmentShader_DiffuseColor;

}

#endif
