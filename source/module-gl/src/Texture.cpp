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

bool bufferGlTextureData2d(GLenum type, Vec2u size, bool is_floating, u32 channels, const void* data){
	GLenum data_format;
	GLenum internal_format;

	if(is_floating){
		switch(channels){
		case 1:
			data_format = GL_RED;
			internal_format = GL_R32F;
			break;
		case 3:
			data_format = GL_RGB;
			internal_format = GL_RGB32F;
			break;
		case 4:
			data_format = GL_RGBA;
			internal_format = GL_RGBA32F;
			break;
		default:
			// :TODO: support 2 channel (greyscale and alpha)
			XenLogError("Invalid texture channel count, got: %i, expected 1,2 or 4");
			return false;
		}
	} else {
		switch(channels){
		case 1: data_format = GL_RED;  break;
		case 2: data_format = GL_RGB;  break;
		case 4: data_format = GL_RGBA; break;
		default:
			// :TODO: support 2 channel (greyscale and alpha)
			XenLogError("Invalid texture channel count, got: %i, expected 1,2 or 4");
			return false;
		}
		internal_format = data_format;
	}

	GLenum data_type = is_floating ? GL_FLOAT : GL_UNSIGNED_BYTE;

	XGL_CHECK(
		glTexImage2D(type,
		             0,                      // mipmap level 0 as this is highest res version
		             internal_format,        // Format used by opengl internally
		             size.x, size.y,
		             0,                      // border - spec says "this value must be 0"
		             data_format, data_type, // format of pixel data
		             data                    // pixel data
		));

	return true;
}

GLenum xgl::getGlTextureType(xen::Texture::Type type){
	switch(type){
	case xen::Texture::Plane   : return GL_TEXTURE_2D;
	case xen::Texture::CubeMap : return GL_TEXTURE_CUBE_MAP;
	}
	return 0;
}

xgl::Texture* doCreateTexture2d(xgl::Texture* result,
                                bool is_floating,
                                u32 channels,
                                Vec2u size,
                                const void* data){
	XenLogDebug("Uploading 2d texture data, size: %ix%i", size.x, size.y);

	if(!bufferGlTextureData2d(GL_TEXTURE_2D, size, is_floating, channels, data)){
		return nullptr;
	}

	// Set texture parameters
	XGL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT));
	XGL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));

	return result;
}

xgl::Texture* doCreateCubeMap(xgl::Texture* result,
                              bool is_floating,
                              u08 channels,
                              Vec3u size,
                              const void** data){
	if(size.z != 6){
		XenLogError("Attempted to create cubemap texture but %i layers provided, expected 6",
		            size.z);
		return nullptr;
	}

	if (size.x != size.y){
		XenLogError("Attempted to create cubemap texture with non-square faces of size %ix%i",
		            size.x, size.y);
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

	XenLogDebug("Uploading cube map data - size: %ix%i", size.x, size.y);

	for(int i = 0; i < 6; ++i){
		if(!bufferGlTextureData2d(TARGETS[i], size.xy, is_floating, channels, data[i])){
			XenLogError("Detected error upon upload of face %i of cubemap", i);
			return nullptr;
		}
	}

	XGL_CHECK(glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE));
	XGL_CHECK(glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE));
	XGL_CHECK(glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE));

	return result;
}

const xen::Texture* xgl::createTexture(xen::Texture::Type type,
                                       bool is_floating,
                                       u08 channels,
                                       Vec3u slice_size,
                                       const void** slice_data){
	// Allocate texture CPU side
	xgl::Texture* texture = xen::reserveType(state->pool_texture);
	texture->type         = type;
	GLenum gl_type        = xgl::getGlTextureType(type);

	// Allocate texture GPU side
	XEN_CHECK_GL(glGenTextures(1, &texture->id));
	XEN_CHECK_GL(glBindTexture(gl_type, texture->id));

	// Do type specific init
	xgl::Texture* result = nullptr;
	switch(type){
	case xen::Texture::Plane:
		result = doCreateTexture2d(texture, is_floating, channels, slice_size.xy, slice_data[0]);
		break;
	case xen::Texture::CubeMap:
		result = doCreateCubeMap(texture, is_floating, channels, slice_size, slice_data);
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
