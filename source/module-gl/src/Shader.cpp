////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of Shader for opengl
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_SHADER_GL_CPP
#define XEN_GL_SHADER_GL_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/core/File.hpp>
#include <xen/kernel/log.hpp>

#include "ModuleGl.hxx"
#include "Shader.hxx"
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

	xgl::ShaderProgram* initShaderProgram(xgl::ShaderProgram* result,
	                                      const char* vertex_source,
	                                      const char* pixel_source
	                                     ){
		result->vertex_shader = compileShader(GL_VERTEX_SHADER,   vertex_source);
		result->pixel_shader  = compileShader(GL_FRAGMENT_SHADER, pixel_source);

		result->program = XEN_CHECK_GL_RETURN(glCreateProgram());

		XEN_CHECK_GL(glAttachShader(result->program, result->vertex_shader  ));
		XEN_CHECK_GL(glAttachShader(result->program, result->pixel_shader));

		XEN_CHECK_GL(glLinkProgram(result->program));

		XEN_CHECK_GL(glDetachShader(result->program, result->vertex_shader  ));
		XEN_CHECK_GL(glDetachShader(result->program, result->pixel_shader));

		////////////////////////////////////////////////////
		// Query Shader Interface
		// :TODO: all this is temp debug code, want to actually store this data somehow...
		GLint attrib_count;

		char tmp[256];

		XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_ATTRIBUTES, &attrib_count));
		XenLogDone("Created shader program");
		XenLogInfo("- Vertex spec has %i attributes", attrib_count);
		for(int i = 0; i < attrib_count; ++i){

			GLint  attrib_size;
			GLenum attrib_type;

			XEN_CHECK_GL(glGetActiveAttrib(result->program, i,
			                               XenArrayLength(tmp), NULL,
			                               &attrib_size, &attrib_type,
			                               tmp));

			GLint attrib_location = XEN_CHECK_GL_RETURN(glGetAttribLocation(result->program, tmp));

			XenLogInfo("  - %2i: %24s, size: %i, type: %6i, location: %2i",
			           i, tmp, attrib_size, attrib_type, attrib_location);
		}

		GLint uniform_count;
		XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_UNIFORMS, &uniform_count));
		XenLogInfo("- Program has %i uniforms", uniform_count);
		for(int i = 0; i < uniform_count; ++i){
			XEN_CHECK_GL(glGetActiveUniformName(result->program, i, XenArrayLength(tmp), NULL, tmp));
		  XenLogInfo("  - %2i: %24s", i, tmp);
		}
		////////////////////////////////////////////////////

		return result;
	}
}

bool xgl::isOkay(xgl::ShaderProgram* shader){
	GLint status_vert, status_pixel, status_prog;

	XEN_CHECK_GL(glGetShaderiv (shader->vertex_shader, GL_COMPILE_STATUS, &status_vert ));
	XEN_CHECK_GL(glGetShaderiv (shader->pixel_shader,  GL_COMPILE_STATUS, &status_pixel));
	XEN_CHECK_GL(glGetProgramiv(shader->program,       GL_LINK_STATUS,    &status_prog ));

	return status_vert == GL_TRUE && status_pixel == GL_TRUE && status_prog == GL_TRUE;
}

char* xgl::getErrors(xgl::ShaderProgram* shader, xen::ArenaLinear& arena){
	if(isOkay(shader)){
		return pushString(arena, "No errors");
	}

	char* result = (char*)arena.next_byte;
	GLint bytes_written;

	xen::pushStringNoTerminate(arena, "Vertex Shader:\n");
	XEN_CHECK_GL(glGetShaderInfoLog(shader->vertex_shader, xen::getBytesRemaining(arena),
			                                &bytes_written, (GLchar*)arena.next_byte));
	xen::ptrAdvance(&arena.next_byte, bytes_written);

	xen::pushStringNoTerminate(arena, "Pixel Shader:\n");
	XEN_CHECK_GL(glGetShaderInfoLog(shader->pixel_shader, xen::getBytesRemaining(arena),
	                                &bytes_written, (GLchar*)arena.next_byte));
	xen::ptrAdvance(&arena.next_byte, bytes_written);

	xen::pushStringNoTerminate(arena, "Program:\n");
	XEN_CHECK_GL(glGetProgramInfoLog(shader->program, xen::getBytesRemaining(arena),
	                                 &bytes_written, (GLchar*)arena.next_byte));
	xen::ptrAdvance(&arena.next_byte, bytes_written);

	return result;
}

void xgl::useShader(xgl::ShaderProgram* shader){
	if(shader == nullptr){
		glUseProgram(0);
	} else {
		glUseProgram(shader->program);
	}
}

int xgl::getUniformLocation(xgl::ShaderProgram* shader, const char* name){
	return glGetUniformLocation(shader->program, name);
}

void xgl::setUniform(int location, Vec3f data){
	XEN_CHECK_GL(glUniform3f(location, data.x, data.y, data.z));
}
void xgl::setUniform(int location, Vec3d data){
	XEN_CHECK_GL(glUniform3d(location, data.x, data.y, data.z));
}
void xgl::setUniform(int location, Vec3u data){
	XEN_CHECK_GL(glUniform3ui(location, data.x, data.y, data.z));
}
void xgl::setUniform(int location, Vec3s data){
	XEN_CHECK_GL(glUniform3i(location, data.x, data.y, data.z));
}
void xgl::setUniform(int location, Vec4f data){
	XEN_CHECK_GL(glUniform4f(location, data.x, data.y, data.z, data.w));
}
void xgl::setUniform(int location, Vec4d data){
	XEN_CHECK_GL(glUniform4d(location, data.x, data.y, data.z, data.w));
}
void xgl::setUniform(int location, Vec4u data){
	XEN_CHECK_GL(glUniform4ui(location, data.x, data.y, data.z, data.w));
}
void xgl::setUniform(int location, Vec4s data){
	XEN_CHECK_GL(glUniform4i(location, data.x, data.y, data.z, data.w));
}
void xgl::setUniform(int location, Mat3f data){
	XEN_CHECK_GL(glUniformMatrix3fv(location, 1, GL_TRUE, data.elements));
}
void xgl::setUniform(int location, Mat3d data){
	XEN_CHECK_GL(glUniformMatrix3dv(location, 1, GL_TRUE, data.elements));
}
void xgl::setUniform(int location, Mat4f data){
	XEN_CHECK_GL(glUniformMatrix4fv(location, 1, GL_TRUE, data.elements));
}
void xgl::setUniform(int location, Mat4d data){
	XEN_CHECK_GL(glUniformMatrix4dv(location, 1, GL_TRUE, data.elements));
}


xen::Shader xgl::createShader(const xen::ShaderSource& source){
	if(source.glsl_vertex_path == nullptr ||
	   source.glsl_fragment_path == nullptr){
		// return handle to the default shader
		return xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(0, 0);
	}

	u32 slot = xen::reserveSlot(xgl::gl_state->pool_shader);
	xen::Shader result = xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(slot, 0);

	XenTempArena(scratch, 8196);

	xen::FileData vertex_src = loadFileAndNullTerminate(scratch, source.glsl_vertex_path);
	xen::FileData pixel_src  = loadFileAndNullTerminate(scratch, source.glsl_fragment_path);

	auto sprog = initShaderProgram(xgl::getShaderImpl(result),
	                               (char*)&vertex_src[0],
	                               (char*)&pixel_src[0]
	                              );

	if(!xgl::isOkay(sprog)){
		resetArena(scratch);
		const char* errors = xgl::getErrors(sprog, scratch);
		XenLogError("Shader Errors:\n%s", errors);
		XenBreak();
	} else {
		XenLogDone("Shader compiled successfully");
	}

	return result;
}

void xgl::destroyShader(xen::Shader shader){
	if(shader._id == 0){
		// never kill the default shader
		return;
	}

	xgl::ShaderProgram* sprog = xgl::getShaderImpl(shader);

	XEN_CHECK_GL(glDeleteShader (sprog->vertex_shader));
	XEN_CHECK_GL(glDeleteShader (sprog->pixel_shader ));
	XEN_CHECK_GL(glDeleteProgram(sprog->program      ));

	xen::freeSlot(xgl::gl_state->pool_shader, shader._id);
}

xgl::ShaderProgram* xgl::getShaderImpl(xen::Shader shader){
	return &xgl::gl_state->pool_shader.slots[shader._id].item;
}

#endif
