////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Unix specific implementation of software render target
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_UNIX_CPP
#define XEN_SREN_RENDERTARGETIMPL_UNIX_CPP

#include "RenderTargetImpl.hxx"
#include "../graphics/Window.hxx"

namespace xen {
	namespace sren {

		void doPlatformRenderTargetInitialization(RenderTargetImpl* target, Window* window) {
			if(window == nullptr){
				// offscreen render targets don't need an associated graphics context
				return;
			}

			XGCValues values;
			GC gc = XCreateGC(xen::impl::unix_display, window->xwindow, 0, &values);

			if(gc < 0) {
				// :TODO: log
				printf("Failed to create graphics context for software render target\n");
				return;
			} else {
				printf("Created graphics context: %li\n", gc);
			}

			target->graphics_context = gc;


			Colormap screen_colormap = DefaultColormap(xen::impl::unix_display,
			                                           DefaultScreen(xen::impl::unix_display));
			XColor red;
			Status rc = XAllocNamedColor(xen::impl::unix_display, screen_colormap, "red", &red, &red);
			if (rc == 0) {
				printf("XAllocNamedColor - failed to allocated 'red' color\n");
			}

			XSetForeground(xen::impl::unix_display, gc, red.pixel);
			XDrawRectangle(xen::impl::unix_display, window->xwindow, gc,
			               10, 10, 100, 100);
		}
	}
}

#endif
