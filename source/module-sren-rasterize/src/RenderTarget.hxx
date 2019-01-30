////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions related to RenderTarget and window
/// management
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_RENDERTARGET_HXX
#define XEN_MODULESRENRASTERIZE_RENDERTARGET_HXX

#include <xen/sren/FragmentShader.hpp>
#include <xen/graphics/GraphicsHandles.hpp>

namespace xen {
	struct Kernel;
	struct Window;
}

namespace xsr {
	struct RenderTarget;

	xsr::RenderTarget* getRenderTargetImpl(xen::RenderTarget target);
	void resizeRenderTarget(xsr::RenderTarget* target, Vec2u size);


	void clear(xen::RenderTarget target, xen::Color color);
	xen::RenderTarget createWindowRenderTarget (xen::Window* window);
	void destroyRenderTarget(xen::RenderTarget render_target);
	void swapBuffers(xen::RenderTarget target);
}

#endif
