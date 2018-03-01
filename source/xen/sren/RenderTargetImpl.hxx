////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the base RenderTargetImpl type and then
/// includes the platform specific header with the true type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_HXX
#define XEN_SREN_RENDERTARGETIMPL_HXX

#include <xen/graphics/Color.hpp>
#include <xen/core/array_types.hpp>
#include <xen/config.hpp>

namespace xen {
	struct Window;

	namespace sren {

		struct RenderTargetImpl;

		/// \brief The values of a single pixel of the various buffers in the
		/// render target
		struct RenderTargetPixel {
			Color4f color;
			float   depth;
		};

		struct RenderTargetImplBase : public Array2d<RenderTargetPixel> {
			/// \brief The window this render target is for (or nullptr if an
			/// off screen render buffer)
			Window* window;
		};

		// https://stackoverflow.com/questions/6609281/how-to-draw-an-image-from-file-on-window-with-xlib

		void doPlatformRenderTargetInitialization(RenderTargetImpl* target, Window* window);
	}
}

#if XEN_OS_UNIX
	#include "RenderTargetImpl.unix.hxx"
#endif

#endif
