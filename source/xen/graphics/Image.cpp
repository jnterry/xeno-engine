////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of types and functions decalred in Image.hpp
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TEXTURE_CPP
#define XEN_GRAPHICS_TEXTURE_CPP

#include <xen/core/memory.hpp>
#include <xen/graphics/Image.hpp>

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

	Color& RawImage::ColRef::operator[](u32 row) {
		return image.pixels[col + (image.height-row) * image.width];
	}

	const Color& RawImage::ColRef::operator[](u32 row) const{
		return image.pixels[col + (image.height-row) * image.width];
	}

	RawImage::ColRef RawImage::operator[](u32 col){
		return { *this, col };
	}

	const RawImage::ColRef RawImage::operator[](u32 col) const{
		return { *const_cast<RawImage*>(this), col };
	}
}

#endif
