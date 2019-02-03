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
	  /// \note These pointers should not be freed as they will point to
	  /// static program wide data (eg: xen::meta_type<float>::type)
	  xen::MetaType* uniform_types;

	  /// \brief Array of hashes of the name of each uniform
	  xen::StringHash* uniform_name_hashes;
  };

	struct Material {
		/// \brief The ShaderProgram used to render this material
		ShaderProgram* shader;


		/// \brief MetaType which describes the layout of the block of client
		/// supplied data to be used for dynamically settable uniforms
		/// \note This object owns the memory pointed at here!
		xen::MetaType* parameter_data_type;

		/// \brief The sources to be used for each uniform
		xen::MaterialParameterSource* uniform_source;
	};

	/// \brief Determines is specified shader program compiled successfully
	bool isOkay(ShaderProgram*);

	/// \brief Pushes a string representing the errors for some program to some arena
	char* getErrors(ShaderProgram*, xen::ArenaLinear& arena);

	/// \brief Makes specified ShaderProgram the active one
	void useShader(ShaderProgram*);

	/// \brief Gets location of specified uniform variable
	int getUniformLocation(ShaderProgram* program, const char* name);

	/// \brief Sets uniform variable at the specified location on active program
	void setUniform(int location, Vec3f data);
	void setUniform(int location, Vec3d data);
	void setUniform(int location, Vec3u data);
	void setUniform(int location, Vec3s data);
	void setUniform(int location, Vec4f data);
	void setUniform(int location, Vec4d data);
	void setUniform(int location, Vec4u data);
	void setUniform(int location, Vec4s data);
	void setUniform(int location, Mat3f data);
	void setUniform(int location, Mat3d data);
	void setUniform(int location, Mat4f data);
	void setUniform(int location, Mat4d data);

	// functions for module interface
	xen::Shader    createShader(const xen::ShaderSource& source);
	void           destroyShader(xen::Shader shader);
	ShaderProgram* getShaderImpl(xen::Shader shader);

	const xen::Material* createMaterial(const xen::ShaderSource& source,
	                                    const xen::Array<xen::MaterialParameterSource>& params);
	void destroyMaterial(const xen::Material*);
}

#endif
