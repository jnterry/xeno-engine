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
#include <xen/graphics/GraphicsHandles.hpp>

xsr::FragmentShader xsr::getShaderImpl(const xen::Shader shader){
	return xsr::state->shader_pool.slots[shader._id].item;
}

xen::Shader xsr::createShader(const void* source){
	u32 slot = xen::reserveSlot(xsr::state->shader_pool);
	xsr::state->shader_pool.slots[slot].item = (xsr::FragmentShader)source;
	return xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(slot, 0);
}

void xsr::destroyShader(xen::Shader shader){
	xen::freeSlot(xsr::state->shader_pool, shader._id);
}


#endif
