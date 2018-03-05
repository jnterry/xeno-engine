////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of platform agnostic functions related
/// to the Window type
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_CPP
#define XEN_GRAPHICS_WINDOW_CPP

#include <xen/graphics/Window.hpp>
#include "Window.hxx"

#include <xen/core/intrinsics.hpp>
#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	// Got an error here?
	// Go grab this file from old version of xeno engine and wrangle it into place
	// shouldn't be too much wrangling required :)
	#include "Window.win.cpp"
#elif defined XEN_OS_UNIX
	#include "Window.unix.cpp"
#else
	// Then use a dummy implementation, this done rather than #error to make
	// porting xeno engine to new platforms easier, since can work on one thing
	// at a time rather than having to implement everything at once (dummy also
	// acts a nice skeleton for the new implementation)
	#warning "Window system not implemented on this platform... using dummy implementation"
	#include "Window.dummy.cpp"
#endif

namespace xen {
	WindowEvent* pollEvent(Window* win){
		impl::dispatchEvents(win);

		if(win->event_front == win->event_first_free){
			return nullptr;
		} else {
			WindowEvent* result = &win->events[win->event_front];
			(++win->event_front) %= XenArrayLength(win->events);
			return result;
		}
	}

	bool isWindowOpen(const Window* win){
		return win->is_open;
	}

	namespace impl {
		bool pushEvent(xen::Window* win, const xen::WindowEvent& event){
			win->events[win->event_first_free] = event;

			(++win->event_first_free) %= XenArrayLength(win->events);
			if(win->event_front == win->event_first_free){
				(++win->event_front) %= XenArrayLength(win->events);
				return false;
			} else {
				return true;
			}
		}
	}
}

#endif
