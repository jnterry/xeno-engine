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

#include <xen/window/Window.hpp>
#include <xen/window/Window.hxx>

#include <xen/core/intrinsics.hpp>
#include <xen/config.hpp>

#include "common.hxx"

xen::WindowEvent* xwn::pollEvent(xen::Window* win){
	if(win->events.size == 0){
		return nullptr;
	} else {
		// Note that we are returning a pointer to something that we cannot
		// guarentee will not be overwritten (eg, if new event comes in).
		// HOWEVER -> new events only arrive during window module's tick()
		// function, and we should be doing event processing inside the tick()
		// function of some other module
		xen::WindowEvent* result = &xen::peakFront(win->events);
		xen::popFront(win->events);
		return result;
	}
}

bool xwn::isWindowOpen(const xen::Window* win){
	return win->state & xen::Window::IS_OPEN;
}
bool xwn::hasFocus(const xen::Window* win){
	return win->state & xen::Window::HAS_FOCUS;
}

bool xwn::pushEvent(xen::Window* win, const xen::WindowEvent& event){
	bool is_full = win->events.capacity == win->events.size;
	if(is_full){
		xen::popFront(win->events);
	}
	xen::pushBack(win->events, event);
	return !is_full;
}


#ifdef XEN_OS_WINDOWS
	#include "win.cpp"
#elif defined XEN_OS_UNIX
	#include "unix.cpp"
#else
	// Then use a dummy implementation, this done rather than #error to make
	// porting xeno engine to new platforms easier, since can work on one thing
	// at a time rather than having to implement everything at once (dummy also
	// acts a nice skeleton for the new implementation)
	#warning "Window system not implemented on this platform... using dummy implementation"
	#include "dummy.cpp"
#endif

#endif
