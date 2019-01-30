////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of opengl unix specific render target
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGETIMPL_WIN_CPP
#define XEN_GL_RENDERTARGETIMPL_WIN_CPP

#include <xen/window/Window.hxx>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/kernel/log.hpp>

namespace xgl {
	struct RenderTargetImpl {
		HGLRC gl_context;
		xen::Window* window;
	};

	void makeCurrent(RenderTargetImpl* target){
		wglMakeCurrent(target->window->context, target->gl_context);
	}

	RenderTargetImpl* createPlatformWindowRenderTarget(xen::ArenaLinear& arena, xen::Window* window){
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

	void destroyPlatformRenderTarget(RenderTargetImpl* target){
		wglDeleteContext(target->gl_context);
	}

	void swapBuffers(xen::RenderTarget handle){
		xgl::RenderTargetImpl* target = getRenderTargetImpl(handle);
		makeCurrent(target); //:TODO: is this needed? shouldnt be here, should be in render()
		SwapBuffers(target->window->context);
	}

}

#endif
