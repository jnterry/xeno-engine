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

#include <xen/math/utilities.hpp>
#include <xen/core/memory/Allocator.hpp>

namespace xen {
	namespace sren {

		void doPlatformRenderTargetInitialization(xen::Allocator* alloc,
		                                          RenderTargetImpl* target,
		                                          Window* window) {
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

			// :TODO: deallocate/resize?
			target->bitmap_pixels    = (char*)alloc->allocate(sizeof(u32) * target->width * target->height);

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

			XFlush(xen::impl::unix_display);
		}

		void presentRenderTarget(Window* window, RenderTargetImpl& target){
			// https://www.linuxquestions.org/questions/programming-9/how-to-draw-a-bitmap-in-the-window-using-xlib-615349/

			// :TODO: this works but is slow...
			// SDL use a shared memory region between the xserver and client, so can
			// just set pixels in that region

			Color4f color;
			u32     color_bits;
			for(u64 x = 0; x < target.rows; ++x){
				for(u64 y = 0; y < target.cols; ++y){
					color = (target[x][y].color);

					color_bits = (xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.r) << window->shift_r |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.g) << window->shift_g |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.b) << window->shift_b
					              );

					XSetForeground(xen::impl::unix_display,
					               target.graphics_context,
					               color_bits
					               );
					XDrawPoint(xen::impl::unix_display,
					           window->xwindow,
					           target.graphics_context,
					           x, target.cols - y);
				}
			}

			XFlush(xen::impl::unix_display);
		}

	}
}

#endif
