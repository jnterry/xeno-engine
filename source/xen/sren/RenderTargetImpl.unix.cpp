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

#include <cstring>

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
			}

			target->graphics_context = gc;

			// :TODO: deallocate/resize?
			target->bitmap_pixels    = (char*)alloc->allocate(sizeof(u32) * target->width * target->height);

			// :TODO: free on render target destruction/resize
			target->ximage = XCreateImage(xen::impl::unix_display,
			                              window->visual,
			                              32,
			                              ZPixmap,
			                              0,
			                              (char*)target->bitmap_pixels,
			                              target->rows,
			                              target->cols,
			                              32,
			                              0
			                              );
			if(!target->ximage){
				printf("Failed to create ximage for render target\n");
			}

			XFlush(xen::impl::unix_display);
		}

		void presentRenderTarget(Window* window, RenderTargetImpl& target){
			// :TODO: this works but is slow...
			// SDL use a shared memory region between the xserver and client, so can
			// just set pixels in that region

			Color4f color;
			u32     color_bits;
			u32* pixels = (u32*)target.bitmap_pixels;
			for(u64 x = 0; x < target.rows; ++x){
				for(u64 y = 0; y < target.cols; ++y){
					color = (target[x][y].color);

					color_bits = (xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.a) << 24 |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.r) << 16 |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.g) <<  8 |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.b) <<  0);

					pixels[y * target.rows + x] = color_bits;
				}
			}

			XPutImage(xen::impl::unix_display,
			          window->xwindow,
			          target.graphics_context,
			          target.ximage,
			          0, 0,
			          0, 0,
			          target.rows, target.cols
			          );
			XFlush(xen::impl::unix_display);
			return;
		}

	}
}

#endif
