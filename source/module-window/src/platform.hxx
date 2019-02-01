////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of functions whose implementations is platform
/// dependent
///
/// \ingroup module-window
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULEWINDOW_PLATFORM_HXX
#define XEN_MODULEWINDOW_PLATFORM_HXX

#include <xen/math/vector_types.hpp>

namespace xen {
	struct Window;
}

namespace xwn {
	Vec2u getClientAreaSize(xen::Window* win);
  void  setWindowTitle   (xen::Window* win, const char* title);
	bool  isKeyPressed     (xen::Key key);

	xen::Window* createWindow(Vec2u size, const char* title);
	void         destroyWindow(xen::Window* window);
}

#endif
