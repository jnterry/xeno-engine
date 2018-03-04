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

		/*::XVisualInfo visual_info;

		/// \brief Number of bits to left shift an 8bit red value by to get it
		/// in the correct location in a 32bit int for display on this window
		u32 shift_r;

		/// \brief Number of bits to left shift an 8bit green value by to get it
		/// in the correct location in a 32bit int for display on this window
		u32 shift_g;

		/// \brief Number of bits to left shift an 8bit blue value by to get it
		/// in the correct location in a 32bit int for display on this window
		u32 shift_b;*/
	};

	namespace impl {
		extern Display* unix_display;
	}
}

#endif
