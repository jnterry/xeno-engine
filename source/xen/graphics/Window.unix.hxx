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

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>

namespace xen {
	struct ArenaLinear;

	struct Window : public xen::impl::WindowBase{
		::Window xwindow;
	};

	namespace impl {
		extern Display* unix_display;

		/// \brief Initializes the unix_display if it has not been yet
		void initUnixDisplay();
	}


}

#endif
