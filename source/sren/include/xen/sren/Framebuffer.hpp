////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of the Framebuffer type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_FRAMEBUFFER_HPP
#define XEN_SREN_FRAMEBUFFER_HPP

#include <xen/graphics/Color.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array_types.hpp>

// :TODO: should this really be part of the public interface?
// Currently needed for adding post processors to a SoftwareDevice.
// Do we want all devices to be able to create framebuffers? add handle type

namespace xen {
	class  Allocator;
	struct RawImage;
}

namespace xsr{
	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a Framebuffer which may be rendered to by software
	/// devices
	/////////////////////////////////////////////////////////////////////
	struct Framebuffer {
		/// \brief Pointer to first element of color buffer
		/// 2d array flattened into 1d as {x0y0, x1y0, y1x0, y1x1}
		xen::Color4f* color;

		/// \brief Pointer to first element of depth buffer
		/// 2d array flattened into 1d as {x0y0, x1y0, y1x0, y1x1}
		float*   depth;

		union {
			struct {
				/// \brief The width  (in pixels) of the framebuffer
				u32 width;

				/// \brief The height (in pixels) of the framebuffer
				u32 height;
			};

			/// \brief Size of the framebuffer in pixels, where x is width and y is height
			Vec2u size;
		};
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new FrameBuffer of specified size
	///
	/// \note The created FrameBuffer should be passed to destroyFrameBuffer
	/// at the end of its life
	///
	/// \param alloc The allocator with which to obtain memory to store the
	/// framebuffer's data
	/// \param Size in pixels of the framebuffer to create
	/// \return Pointer to created FrameBuffer, or nullptr if allocation failed
	/////////////////////////////////////////////////////////////////////
	Framebuffer* createFramebuffer(xen::Allocator& alloc, Vec2u size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Destroys the specified framebuffer
	/// \param alloc The allocator which allocated the framebuffer
	/// \param fb    The frame buffer to destroy
	/////////////////////////////////////////////////////////////////////
	void destroyFramebuffer(xen::Allocator& alloc, Framebuffer* fb);

	/////////////////////////////////////////////////////////////////////
	/// \brief Puts an image on the specified framebuffer
	///
	/// If the image is larger than the framebuffer crops the image but cutting
	/// off right and bottom which is too much
	///
	/// If the image is smaller than the framebuffer, does not change pixel values
	/// outside the image. Places top-left of image at top-left of frame buffer
	///
	/// \param fb        The framebuffer to write to
	/// \param image     The image to blit to the frame buffer
	/// \param depth_val The value to clear the depth value of the frame buffer to
	/////////////////////////////////////////////////////////////////////
	void putImageOnFramebuffer(Framebuffer* fb, const xen::RawImage& image);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the contents of a Framebuffer's color attachment,
	/// storing the result in some RawImage
	///
	/// If the framebuffer is larger than the image crops the framebuffer's
	/// contents, discarding the right most and bottom most pixels
	///
	/// If the framebuffer is smaller than the image, does not change pixels
	/// in the image beyond the extent of the frame buffer. Places top-left of
	/// frame-buffer's contents at top-left of image
	///
	/// \param fb    The framebuffer who's color attachment you wish to retrieve
	/// \param image The image to write to
	/////////////////////////////////////////////////////////////////////
	void getImageFromFramebuffer(const Framebuffer* fb, xen::RawImage& image);
}

#endif
