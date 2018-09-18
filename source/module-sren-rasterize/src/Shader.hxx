////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions related to Shader management
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_SHADER_HXX
#define XEN_MODULESRENRASTERIZE_SHADER_HXX

#include <xen/sren/FragmentShader.hpp>
#include <xen/graphics/GraphicsDevice_types.hpp>

namespace xsr {
	xen::Shader createShader(const void* source);
	void        destroyShader(xen::Shader shader);

	xsr::FragmentShader getShaderImpl(xen::Shader shader);
}

#endif
