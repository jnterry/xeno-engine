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
		////////////////////////////////////////////////////

		XEN_CHECK_GL(glGetProgramiv(result->program, GL_ACTIVE_UNIFORMS, &result->uniform_count));

		GLchar tmp_name_buffer[512];

		xen::MemoryTransaction transaction(xgl::gl_state->primary_arena);

		//////////////////////////////////////////////////////
		// Allocate dynamic sized memory for the ShaderProgram
		u64 data_block_size = result->uniform_count * (
			sizeof(xen::StringHash) + sizeof(xen::MetaType*) + sizeof(GLint)
		);
		void* data_block = xen::reserveBytes(xgl::gl_state->primary_arena, data_block_size);

		result->uniform_locations = (GLint*)data_block;
		xen::ptrAdvance(&data_block, sizeof(GLint) * result->uniform_count);
		result->uniform_types = (xen::MetaType*)data_block;
		xen::ptrAdvance(&data_block, sizeof(xen::MetaType) * result->uniform_count);
		result->uniform_name_hashes = (xen::StringHash*)data_block;
		//////////////////////////////////////////////////////


		//////////////////////////////////////////////////////
		// Retrieve information regarding program uniforms
		XenLogInfo("- Program has %i uniforms", result->uniform_count);
		for(GLint i = 0; i < result->uniform_count; ++i){
			GLint   array_length;
			GLint   name_bytes_written;
			GLenum  type;

			XEN_CHECK_GL(
				glGetActiveUniform(result->program, i,
				                   XenArrayLength(tmp_name_buffer), &name_bytes_written,
				                   &array_length, &type, tmp_name_buffer)
			);

			result->uniform_name_hashes[i] = xen::hash(tmp_name_buffer);
			result->uniform_locations[i] = XEN_CHECK_GL_RETURN(
				glGetUniformLocation(result->program, tmp_name_buffer)
			);

			switch(type){
			case GL_FLOAT:
				XenLogInfo("  - %2i : float       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<float>::type;
				break;
			case GL_FLOAT_VEC2:
				XenLogInfo("  - %2i : Vec2f       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec2f>::type;
				break;
			case GL_FLOAT_VEC3:
				XenLogInfo("  - %2i : Vec3f       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec3f>::type;
				break;
			case GL_FLOAT_VEC4:
				XenLogInfo("  - %2i : Vec4f       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec4f>::type;
				break;
			case GL_DOUBLE:
				XenLogInfo("  - %2i : double      : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<double>::type;
				break;
			case GL_DOUBLE_VEC2:
				XenLogInfo("  - %2i : Vec2d       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec2d>::type;
				break;
			case GL_DOUBLE_VEC3:
				XenLogInfo("  - %2i : Vec3d       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec3d>::type;
				break;
			case GL_DOUBLE_VEC4:
				XenLogInfo("  - %2i : Vec4d       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec4d>::type;
				break;
			case GL_INT:
				XenLogInfo("  - %2i : double      : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<int>::type;
				break;
			case GL_INT_VEC2:
				XenLogInfo("  - %2i : Vec2s       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec2s>::type;
				break;
			case GL_INT_VEC3:
				XenLogInfo("  - %2i : Vec3s       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec3s>::type;
				break;
			case GL_INT_VEC4:
				XenLogInfo("  - %2i : Vec4s       : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<Vec4s>::type;
				break;
			case GL_BOOL:
				XenLogInfo("  - %2i : bool        : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<bool>::type;
				break;
			case GL_FLOAT_MAT4:
				XenLogInfo("  - %2i : Mat4<float> : %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<xen::Matrix<4,4,float>>::type;
				break;
			case GL_DOUBLE_MAT4:
				XenLogInfo("  - %2i : Mat4<double> %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<xen::Matrix<4,4,double>>::type;
				break;
			case GL_SAMPLER_2D:
				XenLogInfo("  - %2i : Sampler2d %s", i, tmp_name_buffer);
				result->uniform_types[i] = xen::meta_type<xen::Texture>::type;
				break;
			default:
				XenLogError("ShaderProgram contained uniform '%s' which has an unsupported type: %i",
				            tmp_name_buffer, type);
					XEN_CHECK_GL(glDeleteShader (result->vertex_shader));
					XEN_CHECK_GL(glDeleteShader (result->pixel_shader ));
					XEN_CHECK_GL(glDeleteProgram(result->program      ));
				return nullptr;
			}
		}


		transaction.commit();
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

const xen::Material* createMaterial(const xen::ShaderSource& source,
                                    const xen::Array<xen::MaterialParameterSource>& params){
	return nullptr;
}
void destroyMaterial(const xen::Material*){

}

#endif
