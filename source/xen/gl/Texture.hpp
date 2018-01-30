////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \file Texture.hpp
/// \author Jamie Terry
/// \date 2018/01/30
/// \brief Contains types and functions for representing Gpu Textures that
/// can be used when rendering
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_TEXTURE_HPP
#define XEN_GL_TEXTURE_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{
	struct RawImage;
	namespace gl {
		/// \brief Opaque type representing texture usable by graphics device
		typedef u32 TextureHandle;

		/// \brief Uploads texture data to graphics device, creating a Texture
		TextureHandle createTexture(RawImage* image);
	}
}

#endif
