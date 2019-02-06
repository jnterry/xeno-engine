////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains interface provided to rest of module for interacting with
/// Materials
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_SHADER_HPP
#define XEN_GL_SHADER_HPP

#include <xen/core/introspection_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include "gl_header.hxx"
#include <xen/graphics/ModuleApiGraphics.hpp>

namespace xgl {
  struct ShaderProgram {
	  GLuint program;       ///< Handle for the program object

	  ///\brief  Number of uniforms in the program
	  GLint uniform_count;

	  /// \brief Array of GLSL location of each uniform
	  GLint*  uniform_locations;

	  /// \brief Array of corresponding CPU types for each GLSL uniform
	  /// \note While the array memory is owned by this object, this actual
	  /// meta types being pointed at should not be freed as they will point to
	  /// static program wide data (eg: xen::meta_type<float>::type)
	  const xen::MetaType** uniform_types;

	  /// \brief GL type for each uniform
	  GLenum* uniform_gl_types;

	  /// \brief Array of hashes of the name of each uniform
	  xen::StringHash* uniform_name_hashes;
  };

	struct Material : xen::Material{
		/// \brief The ShaderProgram used to render this material
		ShaderProgram* program;

		/// \brief Array of the sources to be used for each uniform
		xen::MaterialParameterSource::Kind* uniform_sources;

		/// \brief Offset in bytes into the material parameter struct
		/// that the value of each uniform can be found at
		u32* uniform_param_offsets;
	};

	const xen::Material* createMaterial(const xen::MaterialCreationParameters& params);
	void destroyMaterial(const xen::Material*);

	/// \brief Uses appropriate shader program and sets uniforms in order to begin
	/// using some material. Note that cmd.material is ignored, instead material
	/// is specified separately (this allows overriding the material for module
	/// internal rendering commands, eg, redirecting nullptr to the default
	// material)
	void applyMaterial(const xgl::Material* material,
	                   const xen::RenderCommand3d& cmd,
	                   const xen::RenderParameters3d& params,
	                   const xen::Aabb2u& viewport);

	/// \brief Should be called once at startup to create the default material
	/// to be used by the engine if no other is specified
	const xgl::Material*  initDefaultMaterial();
}

#endif
