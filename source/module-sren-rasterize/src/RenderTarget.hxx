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
#include <xen/graphics/GraphicsDevice_types.hpp>

namespace xen {
	struct Window;
}

namespace xsr {
	struct RenderTarget;

	xsr::RenderTarget* getRenderTargetImpl(xen::RenderTarget target);
	void resizeRenderTarget(xsr::RenderTarget* target, Vec2u size);


	void clear(xen::RenderTarget& target, xen::Color color);
	xen::RenderTarget createRenderTarget (Vec2u size, xen::Window* window);
	void destroyRenderTarget(xen::RenderTarget render_target);
	xen::Window* createWindow(Vec2u size, const char* title);
	void destroyWindow(xen::Window* window);
	void swapBuffers(xen::Window* window);
}

#endif
