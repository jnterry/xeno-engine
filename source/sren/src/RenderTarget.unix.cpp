////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Unix specific implementation of software render target
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_UNIX_CPP
#define XEN_SREN_RENDERTARGETIMPL_UNIX_CPP

#include <xen/window/Window.hxx>

#include <xen/sren/RenderTarget.hxx>
#include <xen/sren/Framebuffer.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/core/memory/Allocator.hpp>

#include <xen/kernel/threads.hpp>

#include <cstring>

namespace {

	void createXImageNonShared(xen::Allocator* alloc,
	                           xsr::RenderTarget& target,
	                           xen::Window* window){

		void* image_data = (u32*)alloc->allocate(sizeof(u32) * target.width * target.height);

		if(!image_data){
			printf("Failed to allocate storage for image pixels\n");
			return;
		}

		target.ximage = XCreateImage(window->xdisplay,
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
	                            xsr::RenderTarget& target,
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
	bool canUseSharedMemory(xen::Window* window){
		const char* display_name = XDisplayName(NULL);
		return ((strncmp(display_name, ":",     1) == 0 ||
		         strncmp(display_name, "unix:", 5) == 0
		         ) &&
		        XShmQueryExtension(window->xdisplay)
		        );
	}

	void createXImageShared(xsr::RenderTarget& target, xen::Window* window){

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
		if(!XShmAttach(window->xdisplay, &target.shminfo)){
			printf("Failed to attach shared memory segment to X Server\n");
			shmdt(target.shminfo.shmaddr);
			// mark segment to be destroyed after last process detaches
			shmctl(target.shminfo.shmid, IPC_RMID, NULL);
			return;
		}

		///////////////////////////////////////////////////////////////////
		// Create the image wrapping the shared memory segment
		XImage* ximage = XShmCreateImage(window->xdisplay,
		                                 window->visual,
		                                 32,                      //depth
		                                 ZPixmap,                 // format
		                                 target.shminfo.shmaddr, // data
		                                 &target.shminfo,        // shared memory info
		                                 target.width, target.height);
		if(!ximage){
			XShmDetach(window->xdisplay, &target.shminfo);
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
	                         xsr::RenderTarget& target,
	                         xen::Window* window){

		if(!target.using_shared_memory){
			destroyXImageNonShared(alloc, target, window);
			return;
		}

		XDestroyImage(target.ximage);

		XShmDetach(window->xdisplay, &target.shminfo);
		shmdt(target.shminfo.shmaddr);
	  shmctl(target.shminfo.shmid, IPC_RMID, NULL);

	  target.ximage = nullptr;
	}
	#endif
}

void xsr::doPlatformRenderTargetInit(xen::Allocator* alloc,
                                     xsr::RenderTarget& target,
                                     xen::Window* window
                                    ){
	if(window == nullptr){
		// offscreen render targets don't need xlib image/graphics context/etc
		return;
	}

	XGCValues values;
	GC gc = XCreateGC(window->xdisplay, window->xwindow, 0, &values);

	if(gc < 0) {
		// :TODO: log
		printf("Failed to create graphics context for software render target\n");
		return;
	}

	target.graphics_context = gc;

	target.ximage        = nullptr;

	#ifndef XEN_NO_XSHM_EXTENSION
	target.using_shared_memory = false;
	if(canUseSharedMemory(window)){
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

	XFlush(window->xdisplay);
}

void xsr::doPlatformRenderTargetDestruction(xen::Allocator* alloc,
                                              xsr::RenderTarget& target,
                                              xen::Window* window){

	if(target.ximage == nullptr){ return; }

	#ifndef XEN_NO_XSHM_EXTENSION
	destroyXImageShared(alloc, target, window);
	#else
	destroyXImageNonShared(alloc, target, window);
	#endif
}

void xsr::doPlatformRenderTargetResize(xen::Allocator* alloc,
                                         xsr::RenderTarget& target,
                                         xen::Window* window) {
	xsr::doPlatformRenderTargetDestruction(alloc, target, window);
	xsr::doPlatformRenderTargetInit(alloc, target, window);
}


namespace {
	struct ThreadGetImageWorkData {
		xen::RawImage       raw_image;
		xsr::RenderTarget target;
	};
	void doThreadPresentRenderTarget(void* voiddata){
		ThreadGetImageWorkData* data = (ThreadGetImageWorkData*)voiddata;
	  xsr::getImageFromFramebuffer(&data->target, data->raw_image);
	}
}
void xsr::presentRenderTarget(xen::Window* window, xsr::RenderTarget& target){

	// Make sure the previous frame has been presented before we go messing
	// with the pixel values...
	XSync(window->xdisplay, True);

	//////////////////////////////////////////////////////////////////////////
	// Update the byte array we show on screen from the float array we do
	// our rendering into
	xen::RawImage raw_image;
	raw_image.size = target.size;
	raw_image.pixels = (xen::Color*)target.ximage->data;

	xen::TickWorkHandle present_group = xen::createTickWorkGroup();

	constexpr u32 WORK_DIVISION_COUNT = 32;
	ThreadGetImageWorkData work_data;
	u32 cur_y   = 0;
	u32 delta_y = target.height / WORK_DIVISION_COUNT;
	for(u32 i = 0; i < WORK_DIVISION_COUNT; ++i){
		xen::RawImage& sub_image = work_data.raw_image;

		sub_image.size.x = raw_image.size.x;
		sub_image.size.y = delta_y;
		sub_image.pixels = &raw_image.pixels[raw_image.size.x * cur_y];

		xsr::RenderTarget& sub_target = work_data.target;
		sub_target.width  = target.width;
		sub_target.height = target.height;
		sub_target.depth  = &target.depth[target.width * cur_y];
		sub_target.color  = &target.color[target.width * cur_y];

		// then this is the last group, claim all remaining pixels if size
		// not divisible exactly by WORK_DIVISION_COUNT
		if(i == WORK_DIVISION_COUNT-1){
			sub_image.size.y = target.height - cur_y;
		}

		xen::pushTickWork(doThreadPresentRenderTarget, &work_data, present_group);

		cur_y += delta_y;
	}
	xen::waitForTickWork(present_group);

	//////////////////////////////////////////////////////////////////////////
	// Put the image on screen
	#ifndef XEN_NO_XSHM_EXTENSION
	if(target.using_shared_memory){
		XShmPutImage(window->xdisplay,
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
		XPutImage(window->xdisplay,
		          window->xwindow,
		          target.graphics_context,
		          target.ximage,
		          0, 0,
		          0, 0,
		          target.width, target.height
		          );
	}
	XFlush(window->xdisplay);
	return;
}

#endif
