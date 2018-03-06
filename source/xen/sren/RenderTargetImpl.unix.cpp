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

namespace {

	void createXImageNonShared(xen::Allocator* alloc,
	                           xen::sren::RenderTargetImpl& target,
	                           xen::Window* window){

		void* image_data = (u32*)alloc->allocate(sizeof(u32) * target.width * target.height);

		if(!image_data){
			printf("Failed to allocate storage for image pixels\n");
			return;
		}

		target.ximage = XCreateImage(xen::impl::unix_display,
		                             window->visual,
		                             32,
		                             ZPixmap,
		                             0, // data offset
		                             (char*)image_data,
		                             target.width, target.height,
		                             32, // padding between scanlines :TODO: -> should be 0?
		                             0 // bytes per line
		                            );

		if(!target.ximage){
			printf("Failed to create x image\n");
			alloc->deallocate(image_data);
			return;
		} else {
			printf("Successfully created non-shared ximage %p, pixels at: %p\n",
			       (void*)target.ximage, image_data);
		}
	}

	void destroyXImageNonShared(xen::Allocator* alloc,
	                            xen::sren::RenderTargetImpl& target,
	                            xen::Window* window){

		// XDestroyImage want's to deallocate the image struct and its data, but
		// we alloced the data with our some xen::Allocator, so shouldn't use free

		// ...so deallocate it ourselves
		alloc->deallocate(target.ximage->data);

		// Prevent XDestroyImage calling free on data
		target.ximage->data = nullptr;
		XDestroyImage(target.ximage);
	}

	#ifndef XEN_NO_XSHM_EXTENSION
	bool canUseSharedMemory(){
		const char* display_name = XDisplayName(NULL);
		return ((strncmp(display_name, ":",     1) == 0 ||
		         strncmp(display_name, "unix:", 5) == 0
		         ) &&
		        XShmQueryExtension(xen::impl::unix_display)
		        );
	}

	void createXImageShared(xen::sren::RenderTargetImpl& target, xen::Window* window){

		// The XShmSegmentInfo struct (target.shminfo) is not created by the
		// XServer, it is really a pack of parameters describing some shared
		// segment which we pass to the XShmXXX functions, hence we can just
		// set the values as we please in this function

		///////////////////////////////////////////////////////////////////
		// Allocate shared memory segment
		target.shminfo.readOnly = False;
		target.shminfo.shmid    = shmget(IPC_PRIVATE,
		                                 sizeof(u32) * target.width * target.height,
		                                 IPC_CREAT | 0777
		                                 );
		if(target.shminfo.shmid < 0){
			// :TODO: log
			printf("Failed to create shared memory segment\n");
			return;
		}

		///////////////////////////////////////////////////////////////////
		// Attach the shared memory segment to this process's address space
		target.shminfo.shmaddr = (char*)shmat(target.shminfo.shmid, NULL, 0);
		if(target.shminfo.shmaddr == (char*)-1){
			printf("Failed to map shared memory segment into process address space\n");

			// mark segment to be destroyed after last process detaches
			shmctl(target.shminfo.shmid, IPC_RMID, NULL);
			return;
		}

		///////////////////////////////////////////////////////////////////
		// Attach the shared memory segment to the X Server's address space
		if(!XShmAttach(xen::impl::unix_display, &target.shminfo)){
			printf("Failed to attach shared memory segment to X Server\n");
			shmdt(target.shminfo.shmaddr);
			// mark segment to be destroyed after last process detaches
			shmctl(target.shminfo.shmid, IPC_RMID, NULL);
			return;
		}

		///////////////////////////////////////////////////////////////////
		// Create the image wrapping the shared memory segment
		XImage* ximage = XShmCreateImage(xen::impl::unix_display,
		                                 window->visual,
		                                 32,                      //depth
		                                 ZPixmap,                 // format
		                                 target.shminfo.shmaddr, // data
		                                 &target.shminfo,        // shared memory info
		                                 target.width, target.height);
		if(!ximage){
			XShmDetach(xen::impl::unix_display, &target.shminfo);
			shmdt(target.shminfo.shmaddr);
			// mark segment to be destroyed after last process detaches
			shmctl(target.shminfo.shmid, IPC_RMID, NULL);
			printf("Failed to create shm ximage\n");
			return;
		}

		// if we've not returned yet then all has gone well, setup the
		// render target
		target.using_shared_memory = true;
		target.ximage              = ximage;
		printf("INFO: Successfully created shared memory XImage %p, pixels at: %p\n",
		       (void*)target.ximage, target.ximage->data
		      );
	}

	void destroyXImageShared(xen::Allocator* alloc,
	                         xen::sren::RenderTargetImpl& target,
	                         xen::Window* window){

		if(!target.using_shared_memory){
			destroyXImageNonShared(alloc, target, window);
			return;
		}

		XDestroyImage(target.ximage);

		XShmDetach(xen::impl::unix_display, &target.shminfo);
		shmdt(target.shminfo.shmaddr);
	  shmctl(target.shminfo.shmid, IPC_RMID, NULL);

	  target.ximage = nullptr;
	}
	#endif
}

namespace xen {
	namespace sren {
		void doPlatformRenderTargetInit(xen::Allocator* alloc, RenderTargetImpl& target, Window* window){
			if(window == nullptr){
				// offscreen render targets don't need xlib image/graphics context/etc
				return;
			}

			XGCValues values;
			GC gc = XCreateGC(xen::impl::unix_display, window->xwindow, 0, &values);

			if(gc < 0) {
				// :TODO: log
				printf("Failed to create graphics context for software render target\n");
				return;
			}

			target.graphics_context = gc;

			target.ximage        = nullptr;

			#ifndef XEN_NO_XSHM_EXTENSION
			target.using_shared_memory = false;
			if(canUseSharedMemory()){
				createXImageShared(target, window);
			}
			#endif

			if(target.ximage == nullptr){
				createXImageNonShared(alloc, target, window);
			}

			if(target.ximage == nullptr){
				// :TODO: log
				printf("ERROR: Failed to create ximage for render target\n");
				return;
			}

			XFlush(xen::impl::unix_display);
		}

		void doPlatformRenderTargetDestruction(xen::Allocator* alloc,
		                                       RenderTargetImpl& target,
		                                       Window* window){

			if(target.ximage == nullptr){ return; }

			#ifndef XEN_NO_XSHM_EXTENSION
			destroyXImageShared(alloc, target, window);
			#else
			destroyXImageNonShared(alloc, target, window);
			#endif
		}

		void doPlatformRenderTargetResize(xen::Allocator* alloc,
		                                  RenderTargetImpl& target,
		                                  Window* window) {
			doPlatformRenderTargetDestruction(alloc, target, window);
			doPlatformRenderTargetInit(alloc, target, window);
		}

		void presentRenderTarget(Window* window, RenderTargetImpl& target){

			// Make sure the previous frame has been presented before we go messing
			// with the pixel values...
			XSync(xen::impl::unix_display, True);


			//////////////////////////////////////////////////////////////////////////
			// Set values of pixels - converting from our float colors to 32bit colors
			Color4f color;
			u32     color_bits;
			// :TODO: take target.red_mask etc into account
			u32* pixels = (u32*)target.ximage->data;
			for(u32 y = 0; y < target.height; ++y){
				u32 base = y * target.width;
				for(u32 x = 0; x < target.width; ++x){
					color = (target.color[base + x]);

					color_bits = (xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.a) << 24 |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.r) << 16 |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.g) <<  8 |
					              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.b) <<  0);

					pixels[target.width*(target.height-y-1) + x] = color_bits;
				}
			}

			//////////////////////////////////////////////////////////////////////////
			// Put the image on screen
			#ifndef XEN_NO_XSHM_EXTENSION
			if(target.using_shared_memory){
				XShmPutImage(xen::impl::unix_display,
				             window->xwindow,
				             target.graphics_context,
				             target.ximage,
				             0, 0,
				             0, 0,
				             target.width, target.height,
				             False);
			} else {
			#else
			{
			#endif
				XPutImage(xen::impl::unix_display,
				          window->xwindow,
				          target.graphics_context,
				          target.ximage,
				          0, 0,
				          0, 0,
				          target.width, target.height
				          );
			}
			XFlush(xen::impl::unix_display);
			return;
		}

	}
}

#endif
