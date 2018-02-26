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

namespace xen{
	namespace gl {
		struct ShaderProgram{
			GLuint vertex_shader; /// \brief handle for the vertex stage
			GLuint pixel_shader;  /// \brief handle for the pixel stage
			GLuint program;       /// \brief handle for the program object
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

			pushStringNoTerminate(arena, "Vertex Shader:\n");
			XEN_CHECK_GL(glGetShaderInfoLog(shader->vertex_shader, bytesRemaining(arena),
			                                &bytes_written, (GLchar*)arena.next_byte));
			ptrAdvance(&arena.next_byte, bytes_written);

			pushStringNoTerminate(arena, "Pixel Shader:\n");
			XEN_CHECK_GL(glGetShaderInfoLog(shader->pixel_shader, bytesRemaining(arena),
			                                &bytes_written, (GLchar*)arena.next_byte));
			ptrAdvance(&arena.next_byte, bytes_written);

			pushStringNoTerminate(arena, "Program:\n");
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
			XEN_CHECK_GL(glUniform3f(location, data.x, data.y, data.z));
		}
		void setUniform(int location, Vec3d data){
			XEN_CHECK_GL(glUniform3d(location, data.x, data.y, data.z));
		}
		void setUniform(int location, Vec3u data){
			XEN_CHECK_GL(glUniform3ui(location, data.x, data.y, data.z));
		}
		void setUniform(int location, Vec3s data){
			XEN_CHECK_GL(glUniform3i(location, data.x, data.y, data.z));
		}
		void setUniform(int location, Vec4f data){
			XEN_CHECK_GL(glUniform4f(location, data.x, data.y, data.z, data.w));
		}
		void setUniform(int location, Vec4d data){
			XEN_CHECK_GL(glUniform4d(location, data.x, data.y, data.z, data.w));
		}
		void setUniform(int location, Vec4u data){
			XEN_CHECK_GL(glUniform4ui(location, data.x, data.y, data.z, data.w));
		}
		void setUniform(int location, Vec4s data){
			XEN_CHECK_GL(glUniform4i(location, data.x, data.y, data.z, data.w));
		}
		void setUniform(int location, Mat3f data){
			XEN_CHECK_GL(glUniformMatrix3fv(location, 1, GL_TRUE, data.elements));
		}
		void setUniform(int location, Mat3d data){
			XEN_CHECK_GL(glUniformMatrix3dv(location, 1, GL_TRUE, data.elements));
		}
		void setUniform(int location, Mat4f data){
			XEN_CHECK_GL(glUniformMatrix4fv(location, 1, GL_TRUE, data.elements));
		}
		void setUniform(int location, Mat4d data){
			XEN_CHECK_GL(glUniformMatrix4dv(location, 1, GL_TRUE, data.elements));
		}
	}
}

#endif
