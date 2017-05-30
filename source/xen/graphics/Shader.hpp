////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Shader.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains functions for creating and using shaders
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SHADER_HPP
#define XEN_GRAPHICS_SHADER_HPP

namespace xen{
	struct ArenaLinear;

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
}

#endif
