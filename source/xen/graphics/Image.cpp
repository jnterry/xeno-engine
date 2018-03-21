////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of types and functions decalred in Image.hpp
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TEXTURE_CPP
#define XEN_GRAPHICS_TEXTURE_CPP

#include <xen/graphics/Image.hpp>
#include <xen/core/memory.hpp>

#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stb_image.hpp>
#include <stb_image_write.hpp>

#include <cstring>

namespace {

	template<typename T>
	xen::RawImage _doLoadImage(T& alloc, const char* file_path){
		// capture output, but last param specified to always give us 4 components anyway
		int width;
		int height;
		int components;

		xen::Color* pixel_data = (xen::Color*)stbi_load(file_path, &width, &height, &components, 4);
		if(pixel_data == nullptr || width < 0 || height < 0 || components < 0){
			//:TODO: log
			printf("Failed to load image file '%s', cause: %s\n", file_path, stbi_failure_reason());
			return {0};
		}

		xen::RawImage result = xen::createImage(alloc, Vec2u{(u32)width, (u32)height});

		if(result.pixels == nullptr){
			// :TODO: log
			printf("Failed to allocate storage for image while loading file: '%s'\n", file_path);
			return {0};
		}

		//stb_image loads images upside down compared to how we want it, so flip rows
		for(u32 y = 0; y < result.height; ++y){
			for(u32 x = 0; x < result.width; ++x){
				result.pixels[y * result.width + x] = pixel_data[(result.height - y - 1) * result.width + x];
			}
		}

		return result;
	}
}

namespace xen{
	Color& RawImage::ColRef::operator[](u32 row) {
		return image.pixels[col + (image.height-row-1) * image.width];
	}

	const Color& RawImage::ColRef::operator[](u32 row) const{
		return image.pixels[col + (image.height-row-1) * image.width];
	}

	RawImage::ColRef RawImage::operator[](u32 col){
		return { *this, col };
	}

	const RawImage::ColRef RawImage::operator[](u32 col) const{
		return { *const_cast<RawImage*>(this), col };
	}

	RawImage createImage(ArenaLinear& arena, Vec2u size){
		xen::MemoryTransaction transaction(arena);

		RawImage result;
		result.pixels = xen::reserveTypeArray<Color>(arena, size.x * size.y);

		if(!xen::isValid(arena)){ return {0}; }
		transaction.commit();

		result.size = size;
		return result;
	}

	RawImage createImage(Allocator& alloc, Vec2u size){
		RawImage result;
		result.size   = size;
		result.pixels = (Color*)alloc.allocate(sizeof(Color) * size.x * size.y);

		if(result.pixels == nullptr){ return {0}; }

		return result;
	}

	void destroyImage(Allocator& alloc, RawImage image){
		alloc.deallocate(image.pixels);
	}

	RawImage loadImage(ArenaLinear& arena, const char* file_path){
		return _doLoadImage(arena, file_path);
	}

	RawImage loadImage(Allocator& alloc, const char* file_path) {
		return _doLoadImage(alloc, file_path);
	}

	bool saveImage(const RawImage& image, const char* file_path, ImageFormat format){

		if(format == ImageFormat::UNKNOWN){
			const char* last_dot = nullptr;
			const char* c = file_path;
			while(*c != '\0'){
				if(*c == '.'              ){ last_dot = c;       }
				if(*c == '/' || *c == '\\'){ last_dot = nullptr; }
				++c;
			}
			if(c == nullptr){
				// :TODO: log
				printf("Failed to determine image format to save as from filename "
				       "since no extension: %s\n", file_path);
				return false;
			}

			++last_dot;
			if       (strcmp(last_dot, "png") == 0){
				format = ImageFormat::PNG;
			} else if(strcmp(last_dot, "bmp") == 0){
					format = ImageFormat::BMP;
			} else if(strcmp(last_dot, "jpg") == 0 || strcmp(last_dot, "jpeg")){
				format = ImageFormat::JPG;
			} else if(strcmp(last_dot, "tga") == 0){
				format = ImageFormat::TGA;
			} else {
				// :TODO: log
				printf("Failed to determine image format to save as from filename "
				       "since unrecongisned extension: %s\n", file_path);
				return false;
			}
		}

	  stbi_flip_vertically_on_write(1);

		int write_result = 0;
		switch(format){
		case ImageFormat::PNG:
			write_result = stbi_write_png(file_path, image.width, image.height, 4, image.pixels, 0);
			break;
		case ImageFormat::BMP:
			write_result = stbi_write_bmp(file_path, image.width, image.height, 4, image.pixels);
			break;
		case ImageFormat::TGA:
			write_result = stbi_write_tga(file_path, image.width, image.height, 4, image.pixels);
			break;
		case ImageFormat::JPG:
			// :TODO: some way to configure, between 0 and 100
			int quality = 100;
			write_result = stbi_write_jpg(file_path, image.width, image.height, 4, image.pixels, quality);
			break;
		}

		if(write_result == 0){
			printf("Failed to write image: %s\n", file_path);
			return false;
		}

		return true;
	}


}

#endif
