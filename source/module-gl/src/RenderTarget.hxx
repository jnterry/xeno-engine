////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declarations of functions related to Render targets
///
/// \ingroup module-gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDERTARGET_HXX
#define XEN_GL_RENDERTARGET_HXX

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/Color.hpp>

namespace xen {
	struct Window;
	struct ArenaLinear;
}

namespace xgl {
	struct RenderTargetImpl;

	void makeCurrent(RenderTargetImpl* target);

	RenderTargetImpl* getRenderTargetImpl(xen::RenderTarget target);
	void clearTarget(xen::RenderTarget target, xen::Color color);

	xen::RenderTarget createWindowRenderTarget(xen::Window* window);
	void destroyRenderTarget                  (xen::RenderTarget target);
	void swapBuffers                          (xen::RenderTarget target);


	void destroyPlatformRenderTarget                  (RenderTargetImpl* target);
	RenderTargetImpl* createPlatformWindowRenderTarget(xen::ArenaLinear& arena, xen::Window* window);
}

#endif
