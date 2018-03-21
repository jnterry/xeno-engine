////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the SoftwareDevice type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICE_HPP
#define XEN_SREN_SOFTWAREDEVICE_HPP

#include <xen/graphics/Color.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array_types.hpp>

namespace xen {
	struct ArenaLinear;
	struct GraphicsDevice;

	namespace sren {
		// :TODO: should this really be part of the public interface? Needed for
		// adding post processors

		// Disable gcc's warning about anonymous structs in unions temporarily...
		#pragma GCC diagnostic push
		#pragma GCC diagnostic ignored "-Wpedantic"

		/////////////////////////////////////////////////////////////////////
		/// \brief Represents a FrameBuffer which may be rendered to by software
		/// devices
		/////////////////////////////////////////////////////////////////////
		struct FrameBuffer {
			/// \brief Pointer to first element of color buffer
			/// 2d array flattened into 1d as {x0y0, x1y0, y1x0, y1x1}
			Color4f* color;

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

		/// \brief Represents a function which performs post processing
		/// on a FrameBuffer before the image is presented to the screen
		typedef void (*PostProcessor)(FrameBuffer& target);
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software raytracer
	/// \param post_processors Array of post processors to call before presenting
	/// the image to the screen. Defaults to empty array
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena,
	                                      xen::Array<sren::PostProcessor> post_processors =
	                                      xen::Array<sren::PostProcessor>::EmptyArray
	                                     );

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software rasterizer
	/// \param post_processors Array of post processors to to call before presenting
	/// the image to the screen. Defaults to an empty array
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<sren::PostProcessor> post_processors =
	                                       xen::Array<sren::PostProcessor>::EmptyArray
	                                      );

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a device which will debug the raytracer device by
	/// drawing the same scene with the rasterizer from multiple angles
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDebugDevice(ArenaLinear& arena);
}

#endif
