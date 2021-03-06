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
#include <xen/graphics/GraphicsHandles.hpp>

namespace xen {
	struct ShaderSource;
}

namespace xsr {
	xen::Shader createShader(const xen::ShaderSource& source);
	void        destroyShader(xen::Shader shader);

	const xen::Material*  createMaterial(const xen::ShaderSource& source,
	                                     const xen::MaterialParameterSource* params,
	                                     u64 param_count);
	void destroyMaterial(const xen::Material* material);

	xsr::FragmentShader getShaderImpl(xen::Shader shader);
}

#endif
