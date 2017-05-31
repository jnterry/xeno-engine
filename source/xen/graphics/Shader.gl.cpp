////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Shader.cpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains implementation of Shader for opengl
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SHADER_GL_CPP
#define XEN_GRAPHICS_SHADER_GL_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/graphics/Shader.hpp>

#include "gl_header.hxx"

namespace {
	GLuint compileShader(GLenum stage, const char* source){
		// create shader
		GLuint result = XEN_CHECK_GL_RETURN(glCreateShader(stage));

		// load source and compile
		XEN_CHECK_GL(glShaderSource(result, 1, &source, NULL));
		XEN_CHECK_GL(glCompileShader(result));

		return result;
	}
}

namespace xen{
	struct ShaderProgram{
		GLuint vertex_shader; /// \breif handle for the vertex stage
		GLuint pixel_shader;  /// \breif handle for the pixel stage
		GLuint program;       /// \breif handle for the program object
	};

	ShaderProgram* createShaderProgram(ArenaLinear& arena, const char* vertex_source, const char* pixel_source){
		xen::MemoryTransaction transaction(arena);

		ShaderProgram* result = xen::reserve<ShaderProgram>(arena);
		result->vertex_shader = compileShader(GL_VERTEX_SHADER,   vertex_source);
		result->pixel_shader  = compileShader(GL_FRAGMENT_SHADER, pixel_source);

		result->program = XEN_CHECK_GL_RETURN(glCreateProgram());

		XEN_CHECK_GL(glAttachShader(result->program, result->vertex_shader  ));
		XEN_CHECK_GL(glAttachShader(result->program, result->pixel_shader));

		XEN_CHECK_GL(glLinkProgram(result->program));

		XEN_CHECK_GL(glDetachShader(result->program, result->vertex_shader  ));
		XEN_CHECK_GL(glDetachShader(result->program, result->pixel_shader));

		transaction.commit();
		return result;
	}

	void destroyShaderProgram(ShaderProgram* shader){
		XEN_CHECK_GL(glDeleteShader (shader->vertex_shader));
		XEN_CHECK_GL(glDeleteShader (shader->pixel_shader ));
		XEN_CHECK_GL(glDeleteProgram(shader->program      ));
	}

	bool isOkay(ShaderProgram* shader){
		GLint status_vert, status_pixel, status_prog;

		XEN_CHECK_GL(glGetShaderiv (shader->vertex_shader, GL_COMPILE_STATUS, &status_vert ));
		XEN_CHECK_GL(glGetShaderiv (shader->pixel_shader,  GL_COMPILE_STATUS, &status_pixel));
		XEN_CHECK_GL(glGetProgramiv(shader->program,       GL_LINK_STATUS,    &status_prog ));

		return status_vert == GL_TRUE && status_pixel == GL_TRUE && status_prog == GL_TRUE;
	}

	char* getErrors(ShaderProgram* shader, ArenaLinear& arena){
		if(isOkay(shader)){
			return pushString(arena, "No errors");
		}

		char* result = (char*)arena.next_byte;

		GLint bytes_written;
		XEN_CHECK_GL(glGetProgramInfoLog(shader->program, bytesRemaining(arena), &bytes_written, (GLchar*)arena.next_byte));
		ptrAdvance(&arena.next_byte, bytes_written);

		return result;
	}

	void useShader(ShaderProgram* shader){
		if(shader == nullptr){
			glUseProgram(0);
		} else {
			glUseProgram(shader->program);
		}
	}

	int getUniformLocation(ShaderProgram* shader, const char* name){
		return glGetUniformLocation(shader->program, name);
	}

	void setUniform(int location, Vec3f data){
		glUniform3f(location, data.x, data.y, data.z);
	}
	void setUniform(int location, Vec3d data){
		glUniform3d(location, data.x, data.y, data.z);
	}
	void setUniform(int location, Vec3u data){
		glUniform3ui(location, data.x, data.y, data.z);
	}
	void setUniform(int location, Vec3s data){
		glUniform3i(location, data.x, data.y, data.z);
	}
	void setUniform(int location, Vec4f data){
		glUniform4f(location, data.x, data.y, data.z, data.w);
	}
	void setUniform(int location, Vec4d data){
		glUniform4d(location, data.x, data.y, data.z, data.w);
	}
	void setUniform(int location, Vec4u data){
		glUniform4ui(location, data.x, data.y, data.z, data.w);
	}
	void setUniform(int location, Vec4s data){
		glUniform4i(location, data.x, data.y, data.z, data.w);
	}
	void setUniform(int location, Mat3f data){
		glUniformMatrix3fv(location, 1, GL_FALSE, data.elements);
	}
	void setUniform(int location, Mat3d data){
		glUniformMatrix3dv(location, 1, GL_FALSE, data.elements);
	}
	void setUniform(int location, Mat4f data){
		glUniformMatrix4fv(location, 1, GL_FALSE, data.elements);
	}
	void setUniform(int location, Mat4d data){
		glUniformMatrix4dv(location, 1, GL_FALSE, data.elements);
	}
}

#endif
