////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of types and functions decalred in Image.hpp
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_IMAGE_CPP
#define XEN_GRAPHICS_IMAGE_CPP

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
			} else if(strcmp(last_dot, "jpg") == 0 || strcmp(last_dot, "jpeg") == 0){
				format = ImageFormat::JPG;
			} else if(strcmp(last_dot, "tga") == 0){
				format = ImageFormat::TGA;
			} else {
				// :TODO: log
				printf("Failed to determine image format to save as from filename "
				       "since unrecognised extension '%s' in: %s\n",
				       last_dot, file_path);
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
		case ImageFormat::JPG: {
			// :TODO: some way to configure, between 0 and 100
			int quality = 100;
			write_result = stbi_write_jpg(file_path, image.width, image.height, 4, image.pixels, quality);
			break;
		}
		case ImageFormat::UNKNOWN:
			XenInvalidCodePath("Type should have been detected from file extension");
			break;
		}

		if(write_result == 0){
			printf("Failed to write image: %s\n", file_path);
			return false;
		}

		return true;
	}
}

Vec3u xen::getCubeMapPixelCoord(Vec3r direction, Vec2u face_size){
	////////////////////////////////////////////////
	// Compute which face is to be sampled
	// This based on which direction has the maximum absolute component
	// and whether that component is positive or negative
	int  max_component = 0;
	real max_abs_component_val = xen::RealMin;
	for(int i = 0; i < 3; ++i){
		real abs_val = abs(direction[i]);
		if(abs_val > max_abs_component_val){
			max_component         = i;
			max_abs_component_val = abs_val;
		}
	}

	xen::CubeMap::Face face = (xen::CubeMap::Face)(
		xen::CubeMap::PositiveX +
		max_component           +
		(direction[max_component] < 0 ? 3 : 0)
	);
	////////////////////////////////////////////////
	// Compute the offset within the face to be sampled
	//
	// We know the other 2 components of direction are less than or equal
	// to direction[max_component], hence this operation modifies their value
	// to varies between -1 and 1
	direction /= max_abs_component_val;

	// Extract just the two coordinates of direction we care about
	Vec2r face_offset;
	switch(max_component){
	case 0:
		face_offset = { direction[1], direction[2] };
		break;
	case 1:
		face_offset = { direction[0], direction[2] };
		break;
	case 2:
		face_offset = { direction[0], direction[1] };
		break;
	}

	// Change from -1 to 1 range to...
	face_offset += { 1.0_r, 1.0_r}; // now between 0 and 2
	face_offset *= 0.5_r;           // now between 0 and 1
	face_offset *= xen::cast<real>(face_size); // now between 0 and face_size

	// Round to nearest pixel center (these start at 0.5 boundaries,
	// hence the -0.5)
	Vec2u face_offset_u = { xen::round32(face_offset.x - 0.5_r),
	                        xen::round32(face_offset.y - 0.5_r) };

	return { face_offset_u.x, face_offset_u.y, face };
}

/// \brief Computes a direction vector from the center of a cube map to some
/// pixel on its surface
/// \param cube_map_pixel Coordinate of a cube map pixel, x and y are the pixel
/// within the face. z represents which face
/// \param face_size The dimensions of each face
Vec3r xen::getCubeMapDirection(Vec3u cube_map_pixel, Vec2u face_size){
	// Compute 0-1 values within the face
	Vec2r face_offset = xen::cast<real>(cube_map_pixel.xy) / xen::cast<real>(face_size);

	// Offset to the center of the pixel (rather than lower left corner)
	face_offset += Vec2r{ 0.5_r / (real)face_size.x, 0.5_r / (real)face_size.y };


	face_offset -= Vec2r{0.5_r, 0.5_r}; // now between -0.5 and 0.5
	face_offset *= 2.0_r; // now between -1 and 1

	switch(cube_map_pixel.z){
	case xen::CubeMap::Face::PositiveX:
		return xen::normalized(Vec3r{  1.0_r, face_offset.x, face_offset.y });
	case xen::CubeMap::Face::NegativeX:
		return xen::normalized(Vec3r{ -1.0_r, face_offset.x, face_offset.y });
	case xen::CubeMap::Face::PositiveY:
		return xen::normalized(Vec3r{ face_offset.x,  1.0_r, face_offset.y });
	case xen::CubeMap::Face::NegativeY:
		return xen::normalized(Vec3r{ face_offset.x, -1.0_r, face_offset.y });
	case xen::CubeMap::Face::PositiveZ:
		return xen::normalized(Vec3r{ face_offset.x, face_offset.y,  1.0_r });
	case xen::CubeMap::Face::NegativeZ:
		return xen::normalized(Vec3r{ face_offset.x, face_offset.y, -1.0_r });
	}

	return Vec3r::Origin;
}

#endif
