////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for creating and using shaders
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

namespace xen{
	struct ArenaLinear;
}

namespace xgl {
  struct ShaderProgram {
	  GLuint vertex_shader; ///< Handle for the vertex stage
	  GLuint pixel_shader;  ///< Handle for the pixel stage
	  GLuint program;       ///< Handle for the program object

	  ///\brief  Number of uniforms in the program
	  GLint uniform_count;

	  /// \brief Array of GLSL location of each uniform
	  GLint*         uniform_locations;

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
	};

	/// \brief Determines is specified shader program compiled successfully
	bool isOkay(ShaderProgram*);

	/// \brief Pushes a string representing the errors for some program to some arena
	char* getErrors(ShaderProgram*, xen::ArenaLinear& arena);

	/// \brief Makes specified ShaderProgram the active one
	void useShader(ShaderProgram*);

	// functions for module interface
	xen::Shader    createShader(const xen::ShaderSource& source);
	void           destroyShader(xen::Shader shader);


	ShaderProgram* getShaderImpl  (xen::Shader shader);

	const xen::Material* createMaterial(const xen::ShaderSource& source,
	                                    const xen::MaterialParameterSource* params,
	                                    u64 param_count);
	void destroyMaterial(const xen::Material*);

	void applyMaterial(const xen::RenderCommand3d& cmd,
	                   const xen::RenderParameters3d& params,
	                   const xen::Aabb2u& viewport);
}

#endif
