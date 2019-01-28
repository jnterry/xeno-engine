////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of functions declared in Window.hxx
///
/// \ingroup module-gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_WINDOW_CPP
#define XEN_GL_WINDOW_CPP

#include "Window.hxx"
#include "RenderTarget.hxx"
#include "ModuleGl.hxx"

#include <xen/graphics/Image.hpp>
#include <xen/core/memory/ArenaPool.hpp>

#if defined XEN_OS_UNIX
	#include "RenderTarget.unix.cpp"
#elif defined XEN_OS_WINDOWS
	#include "RenderTarget.win.cpp"
#else
	// :TODO: add dummy implementation
	#error "GlDevice is not implemented on this platform!"
#endif

namespace xgl {

	xen::Window* createWindow(Vec2u size, const char* title){
		xen::MemoryTransaction transaction(gl_state->primary_arena);
		XenLogDebug("About to create window");

		xen::Window* window = xen::impl::createWindow(gl_state->primary_arena, size, title);

		xen::gl::RenderTargetImpl* render_target =
			xen::gl::createWindowRenderTarget(gl_state->primary_arena, window);

		u32 slot = xen::reserveSlot(gl_state->pool_render_target);
		XenAssert(slot != xen::ArenaPool<xen::gl::RenderTargetImpl*>::BAD_SLOT_INDEX,
		          "Render target pool full");

		gl_state->pool_render_target.slots[slot].item = render_target;
		xen::RenderTarget result = xen::makeGraphicsHandle<xen::RenderTarget::HANDLE_ID>(slot, 0);
		window->render_target = result;

		XenLogDebug("Making render target current");
		xen::gl::makeCurrent(render_target);

		// Initialize glew to get GL extensions
		if(glewInit() != GLEW_OK){
			// :TODO: log
			XenLogError("GLEW Init failed");
			return nullptr;
		}

		// :TODO: log
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

		// :TODO: something better with shaders -> ideally expose them to user of xenogin
		// but how do "programable pipeline" in software / other devices?

		// ensure shader 0 is the default shader
		xgl::createShader({ nullptr, "vertex.glsl", "pixel.glsl" });

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

		return window;
	}

	void destroyWindow(xen::Window* window){
		xen::gl::RenderTargetImpl* target = getRenderTargetImpl(window->render_target);
		xen::gl::destroyRenderTarget(target);
		xen::freeType(gl_state->pool_render_target, &target);

		xen::impl::destroyWindow(window);
		xen::clearToZero<xen::Window>(window);
	}

	void swapBuffers  (xen::Window* window){
		if(window->state & xen::Window::IS_OPEN){
			xen::gl::swapBuffers(getRenderTargetImpl(window->render_target));
		}
	}

}

#endif
