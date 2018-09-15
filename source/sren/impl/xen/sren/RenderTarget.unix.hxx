////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the RenderTargetImpl type for unix
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_UNIX_HXX
#define XEN_SREN_RENDERTARGETIMPL_UNIX_HXX

#include <X11/Xlib.h>

#ifndef XEN_NO_XSHM_EXTENSION
	#include <sys/shm.h>
	#include <X11/extensions/XShm.h>
#endif

namespace xsren {
	struct RenderTarget : public RenderTargetBase {
		// These values need only be set for a render target for a window

		///< \brief The graphics context that may be used to draw to the window
		GC graphics_context;

		///< \brief Meta data about the image format, the `data` member of this
		/// points at the pixel buffer to be shown on screen
		XImage* ximage;

		#ifndef XEN_NO_XSHM_EXTENSION
		/// \brief If set true then ximage.data is a shared memory segment
		/// between the process and x server
		bool using_shared_memory;

		/// \brief Info about the shared memory segment in use
		XShmSegmentInfo shminfo;
		#endif
	};
}

#endif
