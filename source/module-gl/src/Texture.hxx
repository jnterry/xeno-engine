////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for representing Gpu Textures that
/// can be used when rendering
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_TEXTURE_HPP
#define XEN_GL_TEXTURE_HPP

#include "gl_header.hxx"
#include <xen/graphics/Texture_types.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/array_types.hpp>

namespace xen{
	struct RawImage;
}

namespace xgl {
	struct Texture : public xen::Texture {
		/// \brief The GL id of the texture
		GLuint id;
	};

	GLenum getGlTextureType(xen::Texture::Type type);

	const xen::Texture* createTexture (
		xen::Texture::Type type, xen::Array<const xen::RawImage> images
	);

	void destroyTexture(const xen::Texture* texture);
}

#endif
