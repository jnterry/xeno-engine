////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for loading, saving and maninpulating raw
/// images
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_IMAGE_HPP
#define XEN_GRAPHICS_IMAGE_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{
	struct ArenaLinear;

	// gcc doesn't like the anonymous structures inside unions, disable the warning temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents raw image data stored in main memory
	///
	/// \todo :TODO:Replace with Array2d typedefed -> we loose the .size
	/// (although ideally we would add that to Array2d as well)
	/////////////////////////////////////////////////////////////////////
	struct RawImage{
		union{
			struct{ u32 width, height; };
			Vec2u size;
		};
		/// \brief Array of length width*height holding color of each pixel
		Color* pixels;

		/////////////////////////////////////////////////////////////////////
		/// \brief Helper struct used to access pixels of the image
		/////////////////////////////////////////////////////////////////////
		struct ColRef {
			RawImage& image;
			u32       col;

			Color&       operator[](u32 row);
			const Color& operator[](u32 row) const;
		};

		const ColRef operator[](u32 col) const;
		ColRef       operator[](u32 col);
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
}

#endif
