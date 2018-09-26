////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of opengl unix specific render target
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGETIMPL_WIN_CPP
#define XEN_GL_RENDERTARGETIMPL_WIN_CPP

#include <xen/graphics/Window.hxx>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/kernel/log.hpp>

namespace xen {
	namespace gl {
		struct RenderTargetImpl {
			HGLRC gl_context;
			Window* window;
		};

		void makeCurrent(RenderTargetImpl* target){
			wglMakeCurrent(target->window->context, target->gl_context);
		}

		RenderTargetImpl* createWindowRenderTarget(xen::ArenaLinear& arena, xen::Window* window){
			xen::MemoryTransaction transaction(arena);

			RenderTargetImpl* result = xen::reserveType<RenderTargetImpl>(arena);
			if(result == nullptr){ return nullptr; }

			result->gl_context = wglCreateContext(window->context);
			result->window     = window;

			if (result->gl_context == nullptr){
				XenLogError("Failed to create opengl render target for window as failed to create a GL context");
				return nullptr;
			}

			makeCurrent(result);
			XenLogDone("Created OpenGL render target for window, opengl version: '%s'",
			           glGetString(GL_VERSION)
			          );
			return result;
		}

		void destroyRenderTarget(RenderTargetImpl* target){
			wglDeleteContext(target->gl_context);
		}

		void swapBuffers(RenderTargetImpl* target){
			//:TODO: is this needed? shouldnt be here, should be in render()
			makeCurrent(target);
			SwapBuffers(target->window->context);
		}

	}
}

#endif
