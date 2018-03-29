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

	enum class ImageFormat {
		PNG,
		BMP,
		TGA,
		JPG,
		UNKNOWN,
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Saves a RawImage to a file
	/// \param image The RawImage to save
	/// \param file_path The name of the file to save to, any existing file
	/// will be overwritten
	/// \param format The format of the image to save. Defaults to UNKNOWN, which
	/// means attempt to deduce from file extension. If file extension is unknown
	/// then function returns false.
	/// \return True if write succeeded, else false
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	bool saveImage(const RawImage& image, const char* file_path, ImageFormat format = ImageFormat::UNKNOWN);

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



	//////////////////////////////////////////////////////////////////////////////

	// :TODO: support blitting one image to another with different fill types
	// Note: we really also want this for when we present a render target
	// to a window
	/*
	/////////////////////////////////////////////////////////////////////
	/// \brief Enumeration of the supported methods by which a framebuffer
	/// may be displayed on a window's surface.
	/////////////////////////////////////////////////////////////////////
	enum class BlitOperation {
		/////////////////////////////////////////////////////////////////////
		/// \brief The top left of the source and destination buffers will be lined
		/// up
		///
		/// If the source is larger than the destination then extra contents in the
		/// lower right corner will be discarded
		///
		/// If the source is smaller than the target any pixels not covered by the
		/// source in the target will not be modified
		///
		/// For example
		///  # # # . . .
		///  # # # . . .
		///  # # # . . .
		///  # # # . . .
		///  . . . . . .
		///  . . . . . .
		///
		/////////////////////////////////////////////////////////////////////
		TOP_LEFT,

		/////////////////////////////////////////////////////////////////////
		/// \brief The centers' of the source and destination buffers will be lined
		/// up.
		///
		/// If the source is larger than the destination then extra contents along
		/// some dimension then contents will be lost at both edges of the source
		/// on that dimension.
		///
		/// If the source is smaller than the destination then pixels outside of
		/// the source in the destination will not be modified
		///
		/// For example:
		///  . . . . . .
		///  . # # # . .
		///  . # # # . .
		///  . # # # . .
		///  . # # # . .
		///  . . . . . .
		/////////////////////////////////////////////////////////////////////
		CENTER,

		/////////////////////////////////////////////////////////////////////
		/// \brief The centers' of the source and destination buffers will be lined
		/// up, and the source will be scaled such that one dimension is equal to
		/// that of the target, and the other is smaller or equal to that of the
		/// target. Aspect ratio of the source will not be changed.
		///
		///
		/// Pixels outside of the source on the destination will not be modified
		/////////////////////////////////////////////////////////////////////
		CENTER_SCALE_FIT,

		/////////////////////////////////////////////////////////////////////
		/// \brief The center's of the source and destination buffers will be lined
		/// up, and the source will be scaled such that one dimension is equal to
		/// that of the target, and the other is larger or equal to that of that
		/// target. Aspect ratio of the source will not be changed.
		///
		/// Some contents of the source may be lost along the dimension of the
		/// source which is longer than that of the target
		/////////////////////////////////////////////////////////////////////
		CENTER_SCALE_FILL,

		/////////////////////////////////////////////////////////////////////
		/// \brief The source will be stretched along any dimension necessary
		/// in order to fill the destination. Aspect ratio will not be maintained
		/////////////////////////////////////////////////////////////////////
		STRETCH_FILL,
	};
	*/
}

#endif
