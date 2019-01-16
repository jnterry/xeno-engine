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
#include <xen/graphics/Window.hxx>

#include <xen/core/intrinsics.hpp>
#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
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

		if(win->events.size == 0){
			return nullptr;
		} else {
			// Note that we are returning a pointer to something that we cannot
			// guarentee will not be overwritten (eg, if new event comes in).
			// HOWEVER -> we will only push events during impl::dispatchEvents,
			// hence the pointer will remain valid at least until the next call
			// to that function, and that function is called only within pollEvent
			// currently, hence this is safe provided pollEvent is not called before
			// the previous event is processed
			WindowEvent* result = &xen::peakFront(win->events);
			xen::popFront(win->events);
			return result;
		}
	}

	bool isWindowOpen(const Window* win){
		return win->is_open;
	}

	namespace impl {
		bool pushEvent(xen::Window* win, const xen::WindowEvent& event){

			bool is_full = win->events.capacity == win->events.size;
			if(is_full){
				xen::popFront(win->events);
			}

			xen::pushBack(win->events, event);

			return !is_full;
		}
	}
}

#endif
