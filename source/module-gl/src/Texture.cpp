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

GLenum xgl::getGlTextureType(xen::Texture::Type type){
	switch(type){
	case xen::Texture::Plane   : return GL_TEXTURE_2D;
	case xen::Texture::CubeMap : return GL_TEXTURE_CUBE_MAP;
	}
	return 0;
}

xgl::Texture* doCreateTexture2d(xgl::Texture* result,
                                xen::Array<const xen::RawImage> images){
	if(images.size != 1){
		XenLogError("Request to create 2d texture however %i images were provided",
		            images.size);
		return nullptr;
	}

	XenLogDebug("Uploading 2d texture data, size: %ix%i\n", images[0].width, images[0].height);

	//  Upload texture data to GPU
	XEN_CHECK_GL(glTexImage2D(GL_TEXTURE_2D,
	                          0,                         // mipmap level 0 as this is highest res version
	                          GL_RGBA,                   // Internal format for use by GL
	                          images[0].width,           // image size
	                          images[0].height,
	                          0,                         // border - spec says "this value must be 0"
	                          GL_RGBA, GL_UNSIGNED_BYTE, // format of pixel data
	                          images[0].pixels           // pixel data
	             ));

	// Set texture parameters
	XEN_CHECK_GL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT));
	XEN_CHECK_GL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));

	return result;
}

xgl::Texture* doCreateCubeMap(xgl::Texture* result,
                     xen::Array<const xen::RawImage> images){
	if(images.size != 6){
		XenLogError("Request to cube map, but provided with %i images rather than 6", images.size);
		return nullptr;
	}

	static const GLenum TARGETS[6] = {
		GL_TEXTURE_CUBE_MAP_POSITIVE_X,
		GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
		GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
		GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
		GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
		GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
	};

	for(int i = 0; i < 6; ++i){
		//  Upload texture data to GPU
		XEN_CHECK_GL(glTexImage2D(TARGETS[i],
		                          0,                         // mipmap level 0 as this is highest res version
		                          GL_RGBA,                   // Internal format for use by GL
		                          images[0].width,           // image size
		                          images[0].height,
		                          0,                         // border - spec says "this value must be 0"
		                          GL_RGBA, GL_UNSIGNED_BYTE, // format of pixel data
		                          images[0].pixels           // pixel data
		             ));
	}

	return result;
}

const xen::Texture* xgl::createTexture(xen::Texture::Type type,
                                       xen::Array<const xen::RawImage> images){
	// Allocate texture CPU side
	xgl::Texture* texture = xen::reserveType(state->pool_texture);
	texture->type         = type;
	GLenum gl_type = xgl::getGlTextureType(type);

	// Allocate texture GPU side
	XEN_CHECK_GL(glGenTextures(1, &texture->id));
	XEN_CHECK_GL(glBindTexture(gl_type, texture->id));

	// Do type specific init
	xgl::Texture* result = nullptr;
	switch(type){
	case xen::Texture::Plane:
		result = doCreateTexture2d(texture, images);
		break;
	case xen::Texture::CubeMap:
		result = doCreateCubeMap(texture, images);
		break;
	}

	if(result == nullptr){
		XenLogDebug("Texture creation failed, cleaning up GPU resources");
		glDeleteTextures(1, &texture->id);
		return nullptr;
	}

	glGenerateMipmap(gl_type);
	XGL_CHECK(glTexParameterf(gl_type, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR));
	XGL_CHECK(glTexParameterf(gl_type, GL_TEXTURE_MAG_FILTER, GL_LINEAR));

	return result;
}

void xgl::destroyTexture(const xen::Texture* handle){
	xgl::Texture* texture = (xgl::Texture*)handle;

	if(texture->id != 0){
		XEN_CHECK_GL(glDeleteTextures(1, &texture->id));
		texture->id   = 0;
		texture->size = Vec3u::Origin;
	}

	xen::freeType(state->pool_texture, texture);
}

#endif
