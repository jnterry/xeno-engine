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
#include <xen/graphics/Image_types.hpp>

namespace xen{
	struct ArenaLinear;
	class Allocator;

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

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the pixel of a cube map to be accessed given
	/// a direction from the center of the cube map towards one of the faces
	/// \return x and y component will range from 0 to face_size, z component
	/// will range from 0 to 5 to indicate which face should be sampled
	/// (see xen::CubeMap::Face)
	/////////////////////////////////////////////////////////////////////
	Vec3u getCubeMapPixelCoord(Vec3r direction, Vec2u face_size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes a direction vector from the center of a cube map to some
	/// pixel on its surface
	/// \param cube_map_pixel Coordinate of a cube map pixel, x and y are the pixel
	/// within the face. z represents which face
	/// \param face_size The dimensions of each face
	/////////////////////////////////////////////////////////////////////
	Vec3r getCubeMapDirection(Vec3u cube_map_pixel, Vec2u face_size);
}

#endif
