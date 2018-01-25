////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Texture.cpp
/// \author Jamie Terry
/// \date 2017/06/18
/// \brief Contains definition of types and functions decalred in Texture.hpp
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TEXTURE_CPP
#define XEN_GRAPHICS_TEXTURE_CPP

#include <xen/core/memory.hpp>
#include <xen/graphics/Texture.hpp>

#include "gl_header.hxx"

#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stb_image.hpp>
#include <stb_image_write.hpp>

namespace xen{
	RawImage loadImage(ArenaLinear& arena, const char* file_path){
		RawImage result = {0};

		// capture output, but last param specified to always give us 4 components anyway
		int width;
		int height;
		int components;

		void* pixel_data = stbi_load(file_path, &width, &height, &components, 4);
		if(pixel_data == NULL || width < 0 || height < 0 || components < 0){
			//:TODO: log
			printf("Failed to load image file '%s', cause: %s", file_path, stbi_failure_reason());
			return result;
		}

		// data is now loaded
		result.width  = (u32)width;
		result.height = (u32)height;
		result.pixels = (Color*)pixel_data;

		Color temp;

		//stb_image loads images upside down compared to how we want it, so flip rows
		for(u32 row = 0; row < (result.height / 2); ++row){
			u32 base_a = result.width * row;
			u32 base_b = result.width * (result.height - 1 - row);
			for(u32 x = 0; x < result.width; ++x){
				temp = result.pixels[base_a + x];
				result.pixels[base_a + x] = result.pixels[base_b + x];
				result.pixels[base_b + x] = temp;
			}
		}

		return result;
	}

	TextureHandle createTexture(RawImage* image){
		GLuint texture_id;

		//make the texture
		XEN_CHECK_GL(glGenTextures(1, &texture_id));

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

		return (TextureHandle)texture_id;
	}

	Color& RawImage::ColRef::operator[](u32 index) {
		return image.pixels[col + index*image.width];
	}

	const Color& RawImage::ColRef::operator[](u32 index) const{
		return image.pixels[col + index*image.width];
	}

	RawImage::ColRef RawImage::operator[](u32 index){
		return { *this, index };
	}

	const RawImage::ColRef RawImage::operator[](u32 index) const{
		return { *const_cast<RawImage*>(this), index };
	}
}

#endif
