////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Shader.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains functions for creating and using shaders
///
/// \todo :TODO: Eventually we dont want user code to directly be getting uniform
/// locations and setting them - this should be done though some "Material" helper
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_SHADER_HPP
#define XEN_GL_SHADER_HPP

#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>

namespace xen{
	struct ArenaLinear;

	namespace gl {
		///< Opaque type representing a GPU Shader Program
		struct ShaderProgram;

		/// \brief Creates a new ShaderProgram
		ShaderProgram* createShaderProgram(ArenaLinear& arena, const char* vertex_source, const char* pixel_source);

		/// \brief Frees resources associated with specified shader program
		void destroyShaderProgram(ShaderProgram*);

		/// \brief Determines is specified shader program compiled successfully
		bool isOkay(ShaderProgram*);

		/// \brief Pushes a string representing the errors for some program to some arena
		char* getErrors(ShaderProgram*, ArenaLinear& arena);

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
	}
}

#endif
