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
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/core/File.hpp>

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
}

xgl::ShaderProgram* xgl::createShaderProgram(xen::ArenaLinear& arena,
                                        const char* vertex_source,
                                        const char* pixel_source
                                       ){
	xen::MemoryTransaction transaction(arena);

	xgl::ShaderProgram* result = xen::reserveType<xgl::ShaderProgram>(arena);
	result->vertex_shader = compileShader(GL_VERTEX_SHADER,   vertex_source);
	result->pixel_shader  = compileShader(GL_FRAGMENT_SHADER, pixel_source);

	result->program = XEN_CHECK_GL_RETURN(glCreateProgram());

	XEN_CHECK_GL(glAttachShader(result->program, result->vertex_shader  ));
	XEN_CHECK_GL(glAttachShader(result->program, result->pixel_shader));

	XEN_CHECK_GL(glLinkProgram(result->program));

	XEN_CHECK_GL(glDetachShader(result->program, result->vertex_shader  ));
	XEN_CHECK_GL(glDetachShader(result->program, result->pixel_shader));

	transaction.commit();

	////////////////////////////////////////////////////
	// Query Shader Interface
	// :TODO: all this is temp debug code, want to actually store this data somehow...
	GLint attrib_count;

	char tmp[256];

	XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_ATTRIBUTES, &attrib_count));
	printf("Created program\n");
	printf("- Vertex Spec has %i attributes\n", attrib_count);
	for(int i = 0; i < attrib_count; ++i){

		GLint  attrib_size;
		GLenum attrib_type;

		XEN_CHECK_GL(glGetActiveAttrib(result->program, i,
		                               XenArrayLength(tmp), NULL,
		                               &attrib_size, &attrib_type,
		                               tmp));

		GLint attrib_location = XEN_CHECK_GL_RETURN(glGetAttribLocation(result->program, tmp));

		printf("  - %2i: %24s, size: %i, type: %6i, location: %2i\n",
		       i, tmp, attrib_size, attrib_type, attrib_location);
	}

	GLint uniform_count;
	XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_UNIFORMS, &uniform_count));
	printf("- Program has %i uniforms\n", uniform_count);
	for(int i = 0; i < uniform_count; ++i){
		XEN_CHECK_GL(glGetActiveUniformName(result->program, i, XenArrayLength(tmp), NULL, tmp));
		printf("  - %2i: %24s\n", i, tmp);
	}
	////////////////////////////////////////////////////

	return result;
}

void xgl::destroyShaderProgram(xgl::ShaderProgram* shader){
	XEN_CHECK_GL(glDeleteShader (shader->vertex_shader));
	XEN_CHECK_GL(glDeleteShader (shader->pixel_shader ));
	XEN_CHECK_GL(glDeleteProgram(shader->program      ));
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

xgl::ShaderProgram* xgl::loadDefaultShader(xen::ArenaLinear& arena){
	XenTempArena(scratch, 8196);

	// :TODO: we can't rely on these glsl files just existing in bin dir...

	xen::FileData vertex_src = loadFileAndNullTerminate(scratch, "vertex.glsl");
	xen::FileData pixel_src  = loadFileAndNullTerminate(scratch, "pixel.glsl");

	auto result = createShaderProgram(arena,
	                                  (char*)&vertex_src[0],
	                                  (char*)&pixel_src[0]
	                                 );

	if(!xgl::isOkay(result)){
		resetArena(scratch);
		const char* errors = xgl::getErrors(result, scratch);
		printf("Shader Errors:\n%s\n", errors);
		XenBreak();
	} else {
		printf("Shader compiled successfully\n");
	}

	return result;
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

#endif
