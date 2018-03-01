////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of opengl unix specific render target
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGETIMPL_CPP
#define XEN_GL_RENDERTARGETIMPL_CPP

#include "../graphics/Window.unix.hxx"

#include <GL/glx.h>
#include <X11/Xlib.h>

typedef GLXContext (*glXCreateContextAttribsARBProc) (Display*, GLXFBConfig, GLXContext, Bool, const int*);

namespace xen {
	namespace gl {
		struct RenderTargetImpl {
			GLXContext gl_context;

			GLXDrawable drawable;

			xen::Window* window;
		};

		RenderTargetImpl* createWindowRenderTarget(xen::ArenaLinear& arena, xen::Window* window){
			xen::MemoryTransaction transaction(arena);

			// taken from: http://apoorvaj.io/creating-a-modern-opengl-context.html

			RenderTargetImpl* result = xen::reserveType<RenderTargetImpl>(arena);
			result->drawable = window->xwindow;
			result->window   = window;

			int gl_attribs[] = {
				GLX_RENDER_TYPE,   GLX_RGBA_BIT,
				GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
				GLX_DOUBLEBUFFER,  true,
				GLX_RED_SIZE,      1,
				GLX_GREEN_SIZE,    1,
				GLX_BLUE_SIZE,     1,
				GLX_DEPTH_SIZE,    16,
				None
			};

			/////////////////////////////////////////////////////////////////////
			// Get list of supported frame buffer configurations
			int          fb_config_count = 0;
			GLXFBConfig* fb_configs = glXChooseFBConfig(xen::impl::unix_display,
			                                            DefaultScreen(xen::impl::unix_display),
			                                            gl_attribs,
			                                            &fb_config_count
			                                           );
			if (!fb_config_count) {
				// :TODO: log
				printf("ERROR: Failed to get framebuffer config list\n");
				XenBreak();
			} else {
				// :TODO: log
				printf("INFO: Found %i valid framebuffer configurations\n", fb_config_count);
			}


			/////////////////////////////////////////////////////////////////////
			// Create an old GL context so we can get function pointer to
			// glXCreateContextAttribsARBProc
			XVisualInfo* visual_info = glXGetVisualFromFBConfig(xen::impl::unix_display,
			                                           fb_configs[0]
			                                          );
			GLXContext ctx_old = glXCreateContext(xen::impl::unix_display,
			                                      visual_info,
			                                      0,
			                                      GL_TRUE
			                                     );
			glXCreateContextAttribsARBProc glXCreateContextAttribsARB = 0;
			glXCreateContextAttribsARB =
				(glXCreateContextAttribsARBProc)
				glXGetProcAddress((const GLubyte*)"glXCreateContextAttribsARB");

			/////////////////////////////////////////////////////////////////////
			// Destroy the old context
			glXMakeCurrent(xen::impl::unix_display, 0, 0);
			glXDestroyContext(xen::impl::unix_display, ctx_old);
			if (!glXCreateContextAttribsARB) {
				printf("ERROR: glXCreateContextAttribsARB() not found\n");
				XenBreak();
			}

			/////////////////////////////////////////////////////////////////////
			// Set GL Context settings
			int context_attribs[] = {
				// Minimum GL Version
				GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
				GLX_CONTEXT_MINOR_VERSION_ARB, 2,
				None
			};

			/////////////////////////////////////////////////////////////////////
			// Create the GL Context
			result->gl_context = glXCreateContextAttribsARB(xen::impl::unix_display,
			                                                fb_configs[0],
			                                                NULL,
			                                                true,
			                                                context_attribs
			                                               );

			return result;
		}

		void makeCurrent(RenderTargetImpl* target){
			glXMakeCurrent(xen::impl::unix_display,
			               target->drawable,
			               target->gl_context
			              );
		}

		void swapBuffers(RenderTargetImpl* target){
			glXSwapBuffers(xen::impl::unix_display, target->drawable);
		}
	}

}

#endif
