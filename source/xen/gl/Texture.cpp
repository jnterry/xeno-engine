////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains OpenGL specific function for dealing with textures
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_TEXTURE_CPP
#define XEN_GL_TEXTURE_CPP

#include <xen/graphics/Image.hpp>

#include "gl_header.hxx"
#include "Texture.hxx"

namespace xen{
	namespace gl {

		void deleteTexture(TextureImpl* texture){
			if(texture->id != 0){
				XEN_CHECK_GL(glDeleteTextures(1, &texture->id));
				texture->id = 0;
			}
		}

		TextureImpl* loadTexture(const RawImage* image, TextureImpl* texture){

			deleteTexture(texture);

			//make the texture
			XEN_CHECK_GL(glGenTextures(1, &texture->id));

			//set texture parameters
			XEN_CHECK_GL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT));
			XEN_CHECK_GL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));
			XEN_CHECK_GL(glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
			XEN_CHECK_GL(glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));

			//buffer texture data
			XEN_CHECK_GL(glTexImage2D(GL_TEXTURE_2D,
			                          0,                           // mipmap level, 0 since this is highest res version
			                          GL_RGBA,                     // Internal format for use by GL
			                          image->width, image->height, // image size
			                          0,                           // border - "this value must be 0"
			                          GL_RGBA, GL_UNSIGNED_BYTE,   // format of pixel data
			                          image->pixels                // pixel data
			                          ));

			return texture;
		}
	}
}

#endif
