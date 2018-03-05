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

		#ifndef XEN_NO_XSHM_EXTENSION
		bool _canUseSharedMemory(){
			const char* display_name = XDisplayName(NULL);
			return ((strncmp(display_name, ":",     1) == 0 ||
			         strncmp(display_name, "unix:", 5) == 0
			        ) &&
			        XShmQueryExtension(xen::impl::unix_display)
			       );
		}
		void _createXImageShared(RenderTargetImpl* target,
		                         Window* window){

			// The XShmSegmentInfo struct (target->shminfo) is not created by the
			// XServer, it is really a pack of parameters describing some shared
			// segment which we pass to the XShmXXX functions, hence we can just
			// set the values as we please in this function

			///////////////////////////////////////////////////////////////////
			// Allocate shared memory segment
			target->shminfo.readOnly = False;
			target->shminfo.shmid    = shmget(IPC_PRIVATE,
			                                  sizeof(u32) * target->width * target->height,
			                                  IPC_CREAT | 0777
			                                 );
			if(target->shminfo.shmid < 0){
				// :TODO: log
				printf("Failed to create shared memory segment\n");
				return;
			}

			///////////////////////////////////////////////////////////////////
			// Attach the shared memory segment to this process's address space
			target->shminfo.shmaddr = (char*)shmat(target->shminfo.shmid, NULL, 0);
			if(target->shminfo.shmaddr == (char*)-1){
				printf("Failed to map shared memory segment into process address space\n");

				// mark segment to be destroyed after last process detaches
				shmctl(target->shminfo.shmid, IPC_RMID, NULL);
				return;
			}

			///////////////////////////////////////////////////////////////////
			// Attach the shared memory segment to the X Server's address space
			if(!XShmAttach(xen::impl::unix_display, &target->shminfo)){
				printf("Failed to attach shared memory segment to X Server\n");
				shmdt(target->shminfo.shmaddr);
				// mark segment to be destroyed after last process detaches
				shmctl(target->shminfo.shmid, IPC_RMID, NULL);
				return;
			}

			///////////////////////////////////////////////////////////////////
			// Create the image wrapping the shared memory segment
			XImage* ximage = XShmCreateImage(xen::impl::unix_display,
			                                 window->visual,
			                                 32,                      //depth
			                                 ZPixmap,                 // format
			                                 target->shminfo.shmaddr, // data
			                                 &target->shminfo,        // shared memory info
			                                 target->rows, target->cols);
			if(!ximage){
				XShmDetach(xen::impl::unix_display, &target->shminfo);
				shmdt(target->shminfo.shmaddr);
				// mark segment to be destroyed after last process detaches
				shmctl(target->shminfo.shmid, IPC_RMID, NULL);
				printf("Failed to create shm ximage\n");
				return;
			}

			// if we've not returned yet then all has gone well, setup the
			// render target
			target->using_shared_memory = true;
			target->ximage              = ximage;
			target->ximage_pixels       = (u32*)target->shminfo.shmaddr;
			printf("INFO: Successfully created shared memory XImage %li, pixels at: %p\n",
			       target->ximage, target->ximage_pixels
			      );
		}
		#endif

		void _createXImageNonShared(xen::Allocator* alloc,
		                            RenderTargetImpl* target,
		                            Window* window){

			// :TODO: deallocate/resize?
			target->ximage_pixels    = (u32*)alloc->allocate(sizeof(u32) * target->width * target->height);

			if(!target->ximage_pixels){
				printf("Failed to allocate storage for image pixels\n");
				return;
			}

			// :TODO: free on render target destruction/resize
			target->ximage = XCreateImage(xen::impl::unix_display,
			                              window->visual,
			                              32,
			                              ZPixmap,
			                              0, // data offset
			                              (char*)target->ximage_pixels,
			                              target->rows,
			                              target->cols,
			                              32, // padding between scanlines :TODO: -> should be 0?
			                              0 // bytes per line
			                             );

			if(!target->ximage){
				printf("Failed to create x image\n");
				alloc->deallocate(target->ximage_pixels);
				return;
			} else {
				printf("Successfully created non-shared ximage %li, pixels at: %p\n",
				       target->ximage, target->ximage_pixels);
			}

		}

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

			target->ximage              = 0;
			target->ximage_pixels       = nullptr;

			#ifndef XEN_NO_XSHM_EXTENSION
			target->using_shared_memory = false;
			if(_canUseSharedMemory()){
				_createXImageShared(target, window);
			}
			#endif

			if(target->ximage == 0 && target->ximage_pixels == nullptr){
				_createXImageNonShared(alloc, target, window);
			}

			if(!target->ximage || !target->ximage_pixels){
				printf("Failed to create ximage for render target\n");
			}

			XFlush(xen::impl::unix_display);
		}

		void presentRenderTarget(Window* window, RenderTargetImpl& target){

			// Make sure the previous frame has been presented before we go messing
			// with the pixel values...
			XSync(xen::impl::unix_display, True);


			//////////////////////////////////////////////////////////////////////////
			// Set values of pixels - converting from our float colors to 32bit colors
			Color4f color;
			u32     color_bits;
			u32* pixels = (u32*)target.ximage_pixels;
			for(u64 x = 0; x < target.rows; ++x){
				for(u64 y = 0; y < target.cols; ++y){
					color = (target[x][y].color);

					color_bits = (xen::mapToRange<float, u32>(0.0f, 1.0f, 0, 255, color.a) << 24 |
					              xen::mapToRange<float, u32>(0.0f, 1.0f, 0, 255, color.r) << 16 |
					              xen::mapToRange<float, u32>(0.0f, 1.0f, 0, 255, color.g) <<  8 |
					              xen::mapToRange<float, u32>(0.0f, 1.0f, 0, 255, color.b) <<  0);

					pixels[y * target.rows + x] = color_bits;
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
				             target.rows, target.cols,
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
				          target.rows, target.cols
				          );
			}
			XFlush(xen::impl::unix_display);
			return;
		}

	}
}

#endif
