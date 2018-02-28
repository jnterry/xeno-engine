////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of opengl unix specific render target
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGETIMPL_CPP
#define XEN_GL_RENDERTARGETIMPL_CPP

#include <GL/gl.h>
#include <GL/glx.h>

// These are obsolete in most distributions, hence provided explicitly.
#include "GL/glxext.h"
#include "GL/glext.h"

namespace xen {
	namespace gl {
		struct RenderTargetImpl {

		};


		RenderTargetImpl* createRenderTarget(xen::ArenaLinear& arena, xen::Window* window){
			int gl_attr[] = {
				GLX_RGBA,
				GLX_RED_SIZE,    1,
				GLX_GREEN_SIZE,  1,
				GLX_BLUE_SIZE,   1,
				GLX_DOUBLEBUFFER,
				GLX_DEPTH_SIZE,  1,
				None
			};

			Display *d_dpy;
			Window d_win;
			GLXContext d_ctx;

			int elemc;
			GLXFBConfig *fbcfg = glXChooseFBConfig(d_dpy, scrnum, NULL, &elemc);
			if (!fbcfg) {
				printf
			} else {
				printf("Got %d FB configs\n", elemc);
			}

		}
	}

}

#endif
