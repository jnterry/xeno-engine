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

void setTextureSwizzlePattern(GLenum texture_type, u32 channels){
	// OpenGL always internally stores components as R, RG, RGB or RGBA
	// When channel count is 1 or 2 we want r = g = b in shaders to emulate greyscale
	// Hence we set a custom swizzle mask to force this behaviour
	GLint swizzle_mask[4];
	switch(channels){
	case 1:
		swizzle_mask[0] = GL_RED;
		swizzle_mask[1] = GL_RED;
		swizzle_mask[2] = GL_RED;
		swizzle_mask[3] = GL_ONE; // Alpha always 1 if not specified by image
		break;
	case 2:
		swizzle_mask[0] = GL_RED;
		swizzle_mask[1] = GL_RED;
		swizzle_mask[2] = GL_RED;
		swizzle_mask[3] = GL_GREEN; // Alpha specified by image, but as 2nd component
		break;
	case 3:
		swizzle_mask[0] = GL_RED;
		swizzle_mask[1] = GL_GREEN;
		swizzle_mask[2] = GL_BLUE;
		swizzle_mask[3] = GL_ONE; // Alpha always 1 if not specified by image
		break;
	case 4:
		swizzle_mask[0] = GL_RED;
		swizzle_mask[1] = GL_GREEN;
		swizzle_mask[2] = GL_BLUE;
		swizzle_mask[3] = GL_ALPHA;
		break;
	}
	XGL_CHECK(glTexParameteriv(texture_type, GL_TEXTURE_SWIZZLE_RGBA, swizzle_mask));
}

bool bufferGlTextureData2d(GLenum type, Vec2u size, bool is_floating, u32 channels, const void* data){
	GLenum data_format;
	switch(channels){
	case 1: data_format = GL_RED;  break;
	case 2: data_format = GL_RG;   break;
	case 3: data_format = GL_RGB;  break;
	case 4: data_format = GL_RGBA; break;
	default:
		// :TODO: support 2 channel (greyscale and alpha)
		XenLogError("Invalid texture channel count, got: %i, expected 1,2 or 4");
		return false;
	}

	GLenum internal_format;
	if(is_floating){
		// :TODO: can we use lower precision float types (16 or 8)
		// Ideally we want the user to be able to hint to us
		//
		// (we could just let them hint "compressed, normal, high dynamic range"
		// or similar then also apply this to bytes by storing as RGB4 or similar
		// see table 2 on https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml
		switch(channels){
		case 1: internal_format = GL_R32F;    break;
		case 2: internal_format = GL_RG32F;   break;
		case 3: internal_format = GL_RGB32F;  break;
		case 4: internal_format = GL_RGBA32F; break;
		}
	} else {
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
	XenLogDebug("Uploading 2d texture data, size: %ix%i, channels: %i (%s)",
	            size.x, size.y, channels, is_floating ? "floating" : "bytes");

	if(!bufferGlTextureData2d(GL_TEXTURE_2D, size, is_floating, channels, data)){
		return nullptr;
	}

	// Set texture parameters
	XGL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT));
	XGL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));
	setTextureSwizzlePattern (GL_TEXTURE_2D, channels);

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

	XenLogDebug("Uploading cube map data - size: %ix%i, channels %i, floating",
	            size.x, size.y, channels, is_floating);

	for(int i = 0; i < 6; ++i){
		if(!bufferGlTextureData2d(TARGETS[i], size.xy, is_floating, channels, data[i])){
			XenLogError("Detected error upon upload of face %i of cubemap", i);
			return nullptr;
		}
	}

	XGL_CHECK(glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE));
	XGL_CHECK(glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE));
	XGL_CHECK(glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE));
	setTextureSwizzlePattern (GL_TEXTURE_CUBE_MAP, channels);

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
