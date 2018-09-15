////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of functions for window management
///
/// \ingroup module-gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_WINDOW_HXX
#define XEN_GL_WINDOW_HXX

#include <xen/graphics/Window.hpp>

namespace xgl {
	xen::Window* createWindow(Vec2u size, const char* title);
	void destroyWindow       (xen::Window* window);
	void swapBuffers         (xen::Window* window);
}

#endif
