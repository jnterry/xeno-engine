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
}

#endif
