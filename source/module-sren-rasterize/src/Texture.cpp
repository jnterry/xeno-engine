////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of texture management functions
///
/// \ingroup module-sren-texture
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_TEXTURE_CPP
#define XEN_MODULESRENRASTERIZE_TEXTURE_CPP

#include "Texture.hxx"
#include "ModuleRasterize.hxx"
#include <cstring>

xsr::Texture* xsr::getTextureImpl(const xen::Texture texture){
	return &xsr::state->texture_pool.slots[texture._id].item;
}

xen::Texture xsr::createTexture (const xen::RawImage* image){
	u32 slot = xen::reserveSlot(xsr::state->texture_pool);
	xen::Texture result = xen::makeGraphicsHandle<xen::Texture::HANDLE_ID>(slot, 0);
	xsr::Texture* timpl = xsr::getTextureImpl(result);

	u32 num_bytes = sizeof(xen::Color) * image->width * image->height;

	timpl->image.size   = image->size;
	timpl->image.pixels = (xen::Color*)xsr::state->texture_pixel_alloc->allocate(num_bytes);
	memcpy(timpl->image.pixels, image->pixels, num_bytes);

	return result;
}

void xsr::destroyTexture(const xen::Texture texture){
	xsr::Texture* timpl = xsr::getTextureImpl(texture);
	xsr::state->texture_pixel_alloc->deallocate(timpl->image.pixels);
	xen::freeSlot(xsr::state->texture_pool, texture._id);
}


#endif
