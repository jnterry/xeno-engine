////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of opengl unix specific render target
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGETIMPL_UNIX_CPP
#define XEN_GL_RENDERTARGETIMPL_UNIX_CPP

#include <xen/window/Window.hxx>
#include <xen/kernel/log.hpp>

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
				// Ensure we render using RGBA color space rather than palette
				GLX_RENDER_TYPE,   GLX_RGBA_BIT,

				// Ensure we can use the context to draw to an x window
				GLX_X_RENDERABLE,  True,
				GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,

				// Enable double buffering
				GLX_DOUBLEBUFFER,  true,

				// These are minimum values for depth of various buffers GL will pick
				// the max available -> so we just set to 1 to make sure that we don't
				// get a context without a buffer for any of these
				GLX_RED_SIZE,         1,
				GLX_GREEN_SIZE,       1,
				GLX_BLUE_SIZE,        1,
				GLX_ACCUM_RED_SIZE,   1,
				GLX_ACCUM_GREEN_SIZE, 1,
				GLX_ACCUM_BLUE_SIZE,  1,
				GLX_DEPTH_SIZE,       1,
				None
			};

			/////////////////////////////////////////////////////////////////////
			// Get list of supported frame buffer configurations
			int          fb_config_count = 0;
			GLXFBConfig* fb_configs = glXChooseFBConfig(window->display,
			                                            DefaultScreen(window->display),
			                                            gl_attribs,
			                                            &fb_config_count
			                                           );
			if (!fb_config_count) {
				XenLogError("Failed to get framebuffer config list");
				XenBreak();
			} else {
				XenLogInfo("Found %i valid framebuffer configurations", fb_config_count);
			}


			/////////////////////////////////////////////////////////////////////
			// Create an old GL context so we can get function pointer to
			// glXCreateContextAttribsARBProc
			XVisualInfo* visual_info = glXGetVisualFromFBConfig(window->display,
			                                                    fb_configs[0]
			                                                   );
			GLXContext ctx_old = glXCreateContext(window->display,
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
			glXMakeCurrent(window->display, 0, 0);
			glXDestroyContext(window->display, ctx_old);
			if (!glXCreateContextAttribsARB) {
				XenLogError("glXCreateContextAttribsARB() not found");
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
			result->gl_context = glXCreateContextAttribsARB(window->display,
			                                                fb_configs[0],
			                                                NULL,
			                                                true,
			                                                context_attribs
			                                               );

			transaction.commit();
			return result;
		}

		void destroyRenderTarget(RenderTargetImpl* target){
			glXDestroyContext(target->window->display, target->gl_context);
		}

		void makeCurrent(RenderTargetImpl* target){
			glXMakeCurrent(target->window->display,
			               target->drawable,
			               target->gl_context
			              );
		}

		void swapBuffers(RenderTargetImpl* target){
			glXSwapBuffers(target->window->display, target->drawable);
		}
	}

}

#endif
