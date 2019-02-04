////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of platform independent RenderTarget related
/// functions
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGET_CPP
#define XEN_GL_RENDERTARGET_CPP

#include "RenderTarget.hxx"
#include "ModuleGl.hxx"
#include <xen/config.hpp>
#include <xen/window/Window.hxx>
#include <xen/graphics/Image.hpp>

namespace xgl {
	xgl::RenderTargetImpl* getRenderTargetImpl(xen::RenderTarget target){
		return xgl::gl_state->pool_render_target.slots[target._id].item;
	}

	void clearTarget(xen::RenderTarget target_handle, xen::Color color){
		xgl::RenderTargetImpl* target = xgl::getRenderTargetImpl(target_handle);
		xgl::makeCurrent(target);

		xen::Color4f color01 = (xen::Color4f)color;
		XEN_CHECK_GL(glClearColor(color01.r, color01.g, color01.b, 1));
		XEN_CHECK_GL(glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT));
	}

	xen::RenderTarget createWindowRenderTarget(xen::Window* window){
		xen::MemoryTransaction transaction(gl_state->primary_arena);

		xgl::RenderTargetImpl* render_target =
			xgl::createPlatformWindowRenderTarget(gl_state->primary_arena, window);

		u32 slot = xen::reserveSlot(gl_state->pool_render_target);
		XenAssert(slot != xen::ArenaPool<xgl::RenderTargetImpl*>::BAD_SLOT_INDEX,
		          "Render target pool full");

		gl_state->pool_render_target.slots[slot].item = render_target;
		xen::RenderTarget result = xen::makeGraphicsHandle<xen::RenderTarget::HANDLE_ID>(slot, 0);

		XenLogDebug("Making render target current");
		xgl::makeCurrent(render_target);

		// Initialize glew to get GL extensions
		if(glewInit() != GLEW_OK){
			XenLogError("GLEW Init failed");
			return xen::makeNullGraphicsHandle<xen::RenderTarget>();
		}

		int major = 0;
		int minor = 0;
		glGetIntegerv(GL_MAJOR_VERSION, &major);
		glGetIntegerv(GL_MINOR_VERSION, &minor);
		XenLogDone("OpenGL context created:\n - Version %d.%d\n - Vendor %s\n - Renderer %s",
		           major, minor,
		           glGetString(GL_VENDOR),
		           glGetString(GL_RENDERER)
		          );

		XenLogInfo("Performing initial OpenGL setup");
		///////////////////////////////////////////////////
		// Do GL setup

		// :TODO: -> this broken with multiple targets, we need to share textures,
		// programs etc between all targets created by this device
		// A context should be created in module init(), then we share all resources
		// with that

		// Ensure texture 0 is single pixel white
		xen::RawImage image;
		image.size.x = 1;
		image.size.y = 1;
		xen::Color color = xen::Color::WHITE;
		image.pixels = &color;
		xgl::createTexture(&image);

		XEN_CHECK_GL(glEnable   (GL_DEPTH_TEST));
		XEN_CHECK_GL(glDepthFunc(GL_LESS      ));

		XEN_CHECK_GL(glEnable   (GL_CULL_FACE ));
		XEN_CHECK_GL(glFrontFace(GL_CCW       ));
		XEN_CHECK_GL(glCullFace (GL_BACK      ));

		// :TODO: -> needed to use vertex array in core context, but don't know anything
		// more about this than that fact. See:
		// https://stackoverflow.com/a/13405205
		GLuint vao;
		glGenVertexArrays(1, &vao);
		glBindVertexArray(vao);

		//:TODO: compile time if def, 3 levels for gl debugging:
		// none -> no checks
		// some -> do this setup here
		// all  -> enable the XEN_CHECK_GL macros
		//glEnable(GL_DEBUG_OUTPUT);
		//glDebugMessageCallback(&impl::xenGlDebugCallback, nullptr);

		XenLogDone("Completed initial OpenGL setup");

		transaction.commit();
		return result;
	}

	void destroyRenderTarget(xen::RenderTarget handle){
		xgl::RenderTargetImpl* target = getRenderTargetImpl(handle);
		xgl::destroyPlatformRenderTarget(target);
		xen::freeType(gl_state->pool_render_target, &target);
	}
}

#if XEN_OS_UNIX
#include "RenderTarget.unix.cpp"
#elif XEN_OS_WINDOWS
#include "RenderTarget.win.cpp"
#else
#error "No render target impl for this platform"
#endif

#endif
