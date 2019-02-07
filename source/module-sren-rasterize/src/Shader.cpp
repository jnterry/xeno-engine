////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of shader management functions
///
/// \ingroup module-sren-texture
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_SHADER_CPP
#define XEN_MODULESRENRASTERIZE_SHADER_CPP

#include "Shader.hxx"
#include "ModuleRasterize.hxx"

xsr::FragmentShader xsr::getShaderImpl(const xen::Shader shader){
	return xsr::state->shader_pool.slots[shader._id].item;
}

xen::Shader xsr::createShader(const xen::ShaderSource& source){
	u32 slot = xen::reserveSlot(xsr::state->shader_pool);

	xsr::FragmentShader shader = (xsr::FragmentShader)source.sren;
	if(shader == nullptr){
		shader = xsr::FragmentShader_Default;
	}
	xsr::state->shader_pool.slots[slot].item = shader;

	return xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(slot, 0);
}

void xsr::destroyShader(xen::Shader shader){
	xen::freeSlot(xsr::state->shader_pool, shader._id);
}

const xen::Material* xsr::createMaterial(const xen::ShaderSource& source,
                                         const xen::MaterialParameterSource* params,
                                         u64 param_count){
	return nullptr;
}
void xsr::destroyMaterial(const xen::Material* material){
	return;
}


#endif
