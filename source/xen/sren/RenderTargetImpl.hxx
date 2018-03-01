////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the RenderTargetImpl type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGET_HPP
#define XEN_SREN_RENDERTARGET_HPP

#include <xen/core/array_types.hpp>

namespace xen {
	struct Window;

	namespace sren {

		/// \brief The values of a single pixel of the various buffers in the
		/// render target
		struct RenderTargetPixel {
			Color4f color;
			float   depth;
		};

		struct RenderTargetImpl : public Array2d<RenderTargetPixel> {
			/// \brief The window this render target is for (or nullptr if an
			/// off screen render buffer)
			Window* window;
		};

		// https://stackoverflow.com/questions/6609281/how-to-draw-an-image-from-file-on-window-with-xlib
	}
}

#endif
