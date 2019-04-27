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
#include <xen/math/utilities.hpp>

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
			stbi_image_free(pixel_data);
			return {0};
		}

		// Copy over the pixel values into our memory we control
		xen::copyBytes(pixel_data, result.pixels, result.width * result.height * sizeof(xen::Color));

		stbi_image_free(pixel_data);

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

xen::CubeMapUv xen::getCubeMapUv(Vec3r v){
	// Adapted from https://www.gamedev.net/forums/topic/687535-implementing-a-cube-map-lookup-function/
	Vec3r v_abs = {
		xen::abs(v.x), xen::abs(v.y), xen::abs(v.z)
	};
  real ma;

  xen::CubeMapUv result;

	if(v_abs.z >= v_abs.x && v_abs.z >= v_abs.y) {
		result.uv.x = v.z < 0.0 ? -v.x : v.x;
		result.uv.y = -v.y;
		result.face = v.z < 0.0 ? xen::CubeMap::NegativeZ : xen::CubeMap::PositiveZ;
		ma = 0.5 / v_abs.z;
	} else if(v_abs.y >= v_abs.x) {
		result.uv.x = v.x;
		result.uv.y = v.y < 0.0 ? -v.z : v.z;
		result.face = v.y < 0.0 ? xen::CubeMap::NegativeY : xen::CubeMap::PositiveY;
		ma = 0.5 / v_abs.y;
	} else {
		result.uv.x = v.x < 0.0 ? v.z : -v.z;
		result.uv.y = -v.y;
		result.face = v.x < 0.0 ? xen::CubeMap::NegativeX : xen::CubeMap::PositiveX;
	  ma = 0.5 / v_abs.x;
	}

	result.uv *= -ma;
	result.uv.x += 0.5_r;
	result.uv.y += 0.5_r;

	return result;
}

Vec3u xen::getCubeMapPixelCoord(Vec3r v, u32 face_size){
	xen::CubeMapUv val = getCubeMapUv(v);

	return Vec3u{
		(unsigned int)(val.uv.x * face_size),
		(unsigned int)(val.uv.y * face_size),
		(unsigned int)val.face
  };
}

Vec3r xen::getCubeMapDirection(xen::CubeMapUv coord){
	Vec2r face_offset = coord.uv - Vec2r{0.5_r, 0.5_r}; // now between -0.5 and 0.5
	face_offset *= -2.0_r; // now between -1 and 1

	switch(coord.face){
	case xen::CubeMap::Face::PositiveX:
		return xen::normalized(Vec3r{  1.0_r, -face_offset.y, -face_offset.x });
	case xen::CubeMap::Face::NegativeX:
		return xen::normalized(Vec3r{ -1.0_r, -face_offset.y, face_offset.x });
	case xen::CubeMap::Face::PositiveY:
		return xen::normalized(Vec3r{ face_offset.x,  1.0_r, face_offset.y });
	case xen::CubeMap::Face::NegativeY:
		return xen::normalized(Vec3r{ face_offset.x, -1.0_r, -face_offset.y });
	case xen::CubeMap::Face::PositiveZ:
		return xen::normalized(Vec3r{ face_offset.x, -face_offset.y,  1.0_r });
	case xen::CubeMap::Face::NegativeZ:
		return xen::normalized(Vec3r{-face_offset.x, -face_offset.y, -1.0_r });
	}

	return Vec3r::Origin;
}

Vec3r xen::getCubeMapDirection(Vec3u cube_map_pixel, u32 face_size){
	// Compute 0-1 values within the face
	Vec2r face_offset = xen::cast<real>(cube_map_pixel.xy) / (real)face_size;

	// Offset to the center of the pixel (rather than lower left corner)
	face_offset += Vec2r{ 0.5_r / (real)face_size, 0.5_r / (real)face_size };

	return getCubeMapDirection(xen::CubeMapUv{ face_offset, (xen::CubeMap::Face)cube_map_pixel.z});
}

Vec3u xen::getCubeMapPixelNeighbour(Vec3u coord, u32 face_size,
                                    xen::CubeMap::Direction dir){
	switch(dir){
	case xen::CubeMap::Right:
		if(coord.x < face_size-1){
			return { coord.x + 1, coord.y, coord.z };
		}
		switch(coord.z){
		case xen::CubeMap::NegativeZ:
			return { 0, coord.y, xen::CubeMap::PositiveX };
		case xen::CubeMap::PositiveX:
			return { 0, coord.y, xen::CubeMap::PositiveZ };
		case xen::CubeMap::PositiveZ:
			return { 0, coord.y, xen::CubeMap::NegativeX };
		case xen::CubeMap::NegativeX:
			return { 0, coord.y, xen::CubeMap::NegativeZ };
		case xen::CubeMap::PositiveY:
			return { coord.y, face_size-1, xen::CubeMap::NegativeX };
		case xen::CubeMap::NegativeY:
			return { face_size-coord.y-1, 0, xen::CubeMap::NegativeX };
		}
	case xen::CubeMap::Left:
		if(coord.x > 0){
			return { coord.x - 1, coord.y, coord.z };
		}
		switch(coord.z){
		case xen::CubeMap::NegativeZ:
			return { face_size-1, coord.y, xen::CubeMap::NegativeX };
		case xen::CubeMap::PositiveX:
			return { face_size-1, coord.y, xen::CubeMap::NegativeZ };
		case xen::CubeMap::PositiveZ:
			return { face_size-1, coord.y, xen::CubeMap::PositiveX };
		case xen::CubeMap::NegativeX:
			return { face_size-1, coord.y, xen::CubeMap::PositiveZ };
		case xen::CubeMap::PositiveY:
			return { face_size-coord.y-1, face_size-1, xen::CubeMap::PositiveX };
		case xen::CubeMap::NegativeY:
			return { coord.y, 0, xen::CubeMap::PositiveX };
		}
	case xen::CubeMap::Up:
		if(coord.y < face_size-1){
			return { coord.x, coord.y + 1, coord.z };
		}
		switch(coord.z){
		case xen::CubeMap::NegativeZ:
			return { face_size-coord.x-1, coord.y, xen::CubeMap::PositiveY };
		case xen::CubeMap::NegativeX:
			return { face_size-1, coord.x, xen::CubeMap::PositiveY };
		case xen::CubeMap::PositiveZ:
			return { coord.x, 0, xen::CubeMap::PositiveY };
		case xen::CubeMap::PositiveX:
			return { 0, face_size-coord.x-1, xen::CubeMap::PositiveY };
		case xen::CubeMap::PositiveY:
			return { face_size-coord.x-1, face_size-1, xen::CubeMap::NegativeZ };
		case xen::CubeMap::NegativeY:
			return { coord.x, 0, xen::CubeMap::PositiveZ };
		}
	case xen::CubeMap::Down:
		if(coord.y > 0){
			return { coord.x, coord.y - 1, coord.z };
		}
		switch(coord.z){
		case xen::CubeMap::NegativeZ:
			return { face_size-coord.x-1, 0, xen::CubeMap::NegativeY };
		case xen::CubeMap::NegativeX:
			return { face_size-1, face_size-coord.x-1, xen::CubeMap::NegativeY };
		case xen::CubeMap::PositiveZ:
			return { coord.x, face_size-1, xen::CubeMap::NegativeY };
		case xen::CubeMap::PositiveX:
			return { 0, coord.x, xen::CubeMap::NegativeY };
		case xen::CubeMap::PositiveY:
			return { coord.x, face_size-1, xen::CubeMap::PositiveZ };
		case xen::CubeMap::NegativeY:
			return { face_size-coord.x-1, 0, xen::CubeMap::NegativeZ };
		}
	}

	XenBreak("Unhandled case in cube map neighbour");
	return Vec3u::Origin;
}

xen::CubeMapSamplePoints xen::getCubeMapSamplePoints(xen::CubeMapUv coord, u32 face_size){
	xen::CubeMapSamplePoints result;

	// Compute the integer components of the primary pixel (IE: that actually
	// contains the target uv)
	Vec2r pixel = coord.uv * (real)face_size;// - Vec2r{ 0.5_r, 0.5_r };
	Vec3u primary_pixel = { (u32)floor(pixel.x), (u32)floor(pixel.y), (u32)coord.face };

	// Compute the offset from the center of the pixel in question, thus between
	// -0.5 and 0.5 as we go from one edge to the opposite edge
	Vec2r fracs     = pixel - xen::cast<real>(primary_pixel.xy) - Vec2r{ 0.5_r, 0.5_r };
	Vec2r fracs_abs = xen::abs(fracs);

	// Compute the weights to use for adjacent pixels
	result.weight[0] = (1.0 - fracs_abs.x) * (1.0 - fracs_abs.y);
	result.weight[1] = (      fracs_abs.x) * (1.0 - fracs_abs.y);
	result.weight[2] = (1.0 - fracs_abs.x) * (      fracs_abs.y);
	result.weight[3] = (      fracs_abs.x) * (      fracs_abs.y);

	// Work out which direction to step in for u and v relative to the face that
	// contains the primary pixel
	xen::CubeMap::Direction dir_u = fracs.x < 0.0 ? xen::CubeMap::Left : xen::CubeMap::Right;
	xen::CubeMap::Direction dir_v = fracs.y < 0.0 ? xen::CubeMap::Down : xen::CubeMap::Up;

	// Work out whether we will wrap to a different face when stepping along
	// u and along v
	bool wrap_u = (fracs.x < 0.0 && primary_pixel.x <= 0) || (fracs.x > 0.0 && primary_pixel.x >= face_size-1);
	bool wrap_v = (fracs.y < 0.0 && primary_pixel.y <= 0) || (fracs.y > 0.0 && primary_pixel.y >= face_size-1);

	if(wrap_u && wrap_v){
		// Then we are reading from the very corner of a face and are wrapping
		// onto the two adjacent faces
		result.coord[0] = primary_pixel;
		result.coord[1] = xen::getCubeMapPixelNeighbour(result.coord[0], face_size, dir_u);
		result.coord[2] = xen::getCubeMapPixelNeighbour(result.coord[0], face_size, dir_v);

		// dummy value -> weight is 0 so doesnt matter, but we want to ensure it is
		// valid coord as the user code may try to access the cell and multiply by
		// zero, so we still need to ensure the initial read works
		result.coord[3] = result.coord[0];

		// Update weights such that coord[3] doesn't matter and has a weight of 0.0
		//
		// Assuming the primary pixel is bottom left and we are on a corner,
		// then the top right diagonally connected pixel does not exist,
		// hence we want the following transformations to the weight to occur:
		//
		// | 0.00 | 0.00 |        | 0.00 | 0.00 |
		// +------+------+  ===>  +------+------+
		// | 1.00 | 0.00 |        | 1.00 | 0.00 |
		//
		// | 0.25 | 0.25 |        | 0.33 | 0.00 |
		// +------+------+  ===>  +------+------+
		// | 0.25 | 0.25 |        | 0.33 | 0.33 |
		//
		// | 0.50 | 0.00 |        | 0.50 | 0.00 |
		// +------+------+  ===>  +------+------+
		// | 0.50 | 0.00 |        | 0.50 | 0.00 |
		result.weight[0] += result.weight[3] / 3.0;
		result.weight[1] += result.weight[3] / 3.0;
		result.weight[2] += result.weight[3] / 3.0;
		result.weight[3]  = 0.0;
	} else if (wrap_u) {
		// Then we are wrapping u but not v
		// Hence we should first step along v (thus remaining in the face)
		// then along u (which will wrap, hence invalidating dir_u and dir_v)
		result.coord[0] = primary_pixel;
		result.coord[2] = xen::getCubeMapPixelNeighbour(result.coord[0], face_size, dir_v);
		result.coord[1] = xen::getCubeMapPixelNeighbour(result.coord[0], face_size, dir_u);
		result.coord[3] = xen::getCubeMapPixelNeighbour(result.coord[2], face_size, dir_u);
	} else {
		// Then either neither is wrapping (in which case we can step in either
		// order) or just v is wrapping (in which case we should step along u first)
		result.coord[0] = primary_pixel;
		result.coord[1] = xen::getCubeMapPixelNeighbour(result.coord[0], face_size, dir_u);
		result.coord[2] = xen::getCubeMapPixelNeighbour(result.coord[0], face_size, dir_v);
		result.coord[3] = xen::getCubeMapPixelNeighbour(result.coord[1], face_size, dir_v);
	}

	return result;


}

Vec3u xen::getCubeMapPixelCoord(xen::LatLong latlong, u32 face_size){
	Vec3r dir = xen::toCartesian(latlong);
	return getCubeMapPixelCoord(dir, face_size);
}

xen::LatLong xen::getCubeMapLatLong(Vec3u coord, u32 face_size){
	Vec3r dir = xen::getCubeMapDirection(coord, face_size);
	return xen::toLatLong(dir);
}

#endif
