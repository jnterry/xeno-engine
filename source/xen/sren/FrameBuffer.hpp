////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of the FrameBuffer type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_FRAMEBUFFER_HPP
#define XEN_SREN_FRAMEBUFFER_HPP

#include <xen/graphics/Color.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array_types.hpp>

// :TODO: should this really be part of the public interface? Needed for
// adding post processors to a SoftwareDevice. Works for now.

namespace xen{
	namespace sren {
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
	}
}

#endif
