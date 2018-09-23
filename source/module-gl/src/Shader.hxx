////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for creating and using shaders
///
/// \todo :TODO: Eventually we dont want user code to directly be getting uniform
/// locations and setting them - this should be done though some "Material" helper
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_SHADER_HPP
#define XEN_GL_SHADER_HPP

#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include "gl_header.hxx"
#include <xen/graphics/GraphicsModuleApi.hpp>

namespace xen{
	struct ArenaLinear;
}

namespace xgl {
  struct ShaderProgram {
	  GLuint vertex_shader; ///< Handle for the vertex stage
	  GLuint pixel_shader;  ///< Handle for the pixel stage
	  GLuint program;       ///< Handle for the program object
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
}

#endif
