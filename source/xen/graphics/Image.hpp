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
	class Allocator;

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
	/// \brief Creates a new image of the specified size.
	///
	/// This function does not initialise the pixels of the image to any color
	///
	/// \note The created image should be passed to destroyImage in order to free
	/// the allocated memory
	///
	/// \param alloc The allocator with which to allocated the bytes to store
	/// the image
	/// \param size Size of the image to create in pixels
	/// \return Created raw image, will be cleared to 0 if allocation failed
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage createImage(Allocator& alloc, Vec2u size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new image of the specified size.
	///
	/// This function does not initialise the pixels of the image to any color
	///
	/// \note The pixel data for the created image will be stored in the specified
	/// ArenaLinear. No operation needs to be performed to free this memory
	///
	/// \param arena The arena to allocate the pixel data in
	/// \param size  The dimensions in pixels of the image to create
	/// \return Created raw image, will be cleared to 0 if allocation failed
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage createImage(ArenaLinear& arena, Vec2u size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads an image from the specified file
	/// \param arena     Arena within with pixel data will be stored
	/// \param file_path Path to the file to load
	/// \return RawImage by value, will be cleared to 0 if load failed
	///
	/// /// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage loadImage(ArenaLinear& arena, const char* file_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads an image from the specified file
	///
	/// \note The created image should be passed to destroyImage in order to free
	/// the allocated memory
	///
	/// \param alloc     Allocator with which to allocate bytes to store pixel data
	/// \param file_path Path to the file to load
	/// \return RawImage by value, will be cleared to 0 if load failed
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage loadImage(Allocator& alloc, const char* file_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Saves a RawImage to a file
	/// \param image The RawImage to save
	/// \param file_path The name of the file to save to, any existing file
	/// will be overwritten
	/// \return True if write succeeded, else false
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	bool saveImage(const RawImage& image, const char* file_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Frees the memory allocated for the specified image.
	///
	/// \note This function should only be called if the image was created
	/// or loaded with the specified Allocator providing the memory for the
	/// image's pixels.
	/// \param alloc The allocator which allocated this image
	/// \param image The image to destroy
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	void destroyImage(Allocator& alloc, RawImage image);
}

#endif
