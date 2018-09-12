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

#include <xen/core/intrinsics.hpp>

#include "gl_header.hxx"

namespace xen{
	struct RawImage;
	namespace gl {

		struct TextureImpl {
			/// \brief The GL id of the texture
			GLuint id;
		};

		/// \brief Uploads texture data to graphics card, filling in specified
		/// texture struct
		/// \return Pointer to texture parameter
	  TextureImpl* loadTexture(const RawImage* image, TextureImpl* texture);

		/// \brief Deletes a texture from graphics card, invalidates passed
		/// in texture object
		void deleteTexture(TextureImpl* texture);
	}
}

#endif
