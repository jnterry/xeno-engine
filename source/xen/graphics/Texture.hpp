////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Texture.hpp
/// \author Jamie Terry
/// \date 2017/06/18
/// \brief Contains types and functions for manipulating Textures
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TEXTURE_HPP
#define XEN_GRAPHICS_TEXTURE_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/Vector.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{
	struct ArenaLinear;

	/// \brief Opaque type representing texture usable by graphics device
	typedef u32 TextureHandle;

	// gcc doesn't like the anonymous structures inside unions, disable the warning temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/// \brief Represents raw image data stored in CPU memory
	struct RawImage{
		union{
			struct{ u32 width, height; };
			Vec2u size;
		};
		/// \brief Array of length width*height holding color of each pixel
		Color* pixels;
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic


	/////////////////////////////////////////////////////////////////////
	/// \brief Loads an image from the specified file
	/// \param arena     Arena within with pixel data will be stored
	/// \param file_path Path to the file to load
	/// \return RawImage by value, will be cleared to 0 if load failed
	/////////////////////////////////////////////////////////////////////
	RawImage loadImage(ArenaLinear& arena, const char* file_path);

	enum class ImageType {
		PNG,
		BMP,
		TGA,
		HDR,
		JPG,
		UNKNOWN,
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Saves a RawImage to a file
	/// \param image    The image's pixel values
	/// \param filename The name of the file to save the image to
	/////////////////////////////////////////////////////////////////////
	bool     saveImage(const RawImage* image, const char* filename);

	/// \brief Uploads texture data to graphics device, creating a Texture
	TextureHandle createTexture(RawImage* image);
}

#endif
