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
#include "ModuleGl.hxx"

xgl::TextureImpl* xgl::getTextureImpl(const xen::Texture texture){
	return &gl_state->pool_texture.slots[texture._id].item;
}

xen::Texture xgl::createTexture(const xen::RawImage* image){
	// Allocate texture CPU side
	u32 slot = xen::reserveSlot(gl_state->pool_texture);
	xen::Texture result = xen::makeGraphicsHandle<xen::Texture::HANDLE_ID>(slot, 0);
	xgl::TextureImpl* timpl = xgl::getTextureImpl(result);

	// Allocate texture GPU side
	XEN_CHECK_GL(glGenTextures(1, &timpl->id));
	XEN_CHECK_GL(glBindTexture(GL_TEXTURE_2D, timpl->id));

	// Set texture parameters
	XEN_CHECK_GL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT));
	XEN_CHECK_GL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));
	XEN_CHECK_GL(glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
	XEN_CHECK_GL(glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));

	//  Upload texture data to GPU
	XEN_CHECK_GL(glTexImage2D(GL_TEXTURE_2D,
	                          0,                           // mipmap level 0 as this is highest res version
	                          GL_RGBA,                     // Internal format for use by GL
	                          image->width, image->height, // image size
	                          0,                           // border - "this value must be 0"
	                          GL_RGBA, GL_UNSIGNED_BYTE,   // format of pixel data
	                          image->pixels                // pixel data
	                         ));

	return result;
}

void xgl::destroyTexture(const xen::Texture texture){
  xgl::TextureImpl* timpl = getTextureImpl(texture);

	if(timpl->id != 0){
		XEN_CHECK_GL(glDeleteTextures(1, &timpl->id));
	}

	xen::freeType(gl_state->pool_texture, timpl);
}

#endif
