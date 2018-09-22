////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declerations of functions related to Render targets
///
/// \ingroup module-gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDER_TARGET_HXX
#define XEN_GL_RENDER_TARGET_HXX

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/Color.hpp>

namespace xen {
	namespace gl {
		struct RenderTargetImpl;
		void makeCurrent(RenderTargetImpl* target);
	}
}

namespace xgl {
	xen::gl::RenderTargetImpl* getRenderTargetImpl(xen::RenderTarget target);
	void clearTarget(xen::RenderTarget target, xen::Color color);
}

#endif
