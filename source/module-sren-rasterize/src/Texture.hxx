////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions related to Texture management
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_TEXTURE_HXX
#define XEN_MODULESRENRASTERIZE_TEXTURE_HXX

#include <xen/sren/Texture.hpp>
#include <xen/graphics/GraphicsHandles.hpp>

namespace xsr {
	xsr::Texture* getTextureImpl(const xen::Texture texture);
	xen::Texture  createTexture (const xen::RawImage* image);
	void          destroyTexture(const xen::Texture texture);
}

#endif
