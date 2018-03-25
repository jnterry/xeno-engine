////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains windows specific decleration of window type
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_WIN_HXX
#define XEN_GRAPHICS_WINDOW_WIN_HXX

#include "Window.hxx"

#include <xen/windows_header.hxx>

namespace xen {
	struct Window : public xen::impl::WindowBase{
		HWND handle;
		HDC  context;
	};
}

#endif
