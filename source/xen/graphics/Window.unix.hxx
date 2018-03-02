////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_UNIX_HXX
#define XEN_GRAPHICS_WINDOW_UNIX_HXX

#include "Window.hxx"

#include <X11/Xlib.h>
#include <X11/Xutil.h>

namespace xen {
	struct ArenaLinear;

	struct Window : public xen::impl::WindowBase{
		::Window xwindow;

		::Visual* visual;

		::XVisualInfo visual_info;
	};

	namespace impl {
		extern Display* unix_display;
	}
}

#endif
