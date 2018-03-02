////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the RenderTargetImpl type for unix
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_UNIX_HXX
#define XEN_SREN_RENDERTARGETIMPL_UNIX_HXX

#include <X11/Xlib.h>

namespace xen {
	namespace sren {

		struct RenderTargetImpl : public RenderTargetImplBase {
			// https://tronche.com/gui/x/xlib/GC/XCreateGC.html
			GC graphics_context;

			XImage image;

			char* bitmap_pixels;
		};
	}
}

#endif
