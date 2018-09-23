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

namespace xgl {
	xen::gl::RenderTargetImpl* getRenderTargetImpl(xen::RenderTarget target){
		return xgl::gl_state->pool_render_target.slots[target._id].item;
	}

	void clearTarget(xen::RenderTarget target_handle, xen::Color color){
		xen::gl::RenderTargetImpl* target = xgl::getRenderTargetImpl(target_handle);
		xen::gl::makeCurrent(target);

		xen::Color4f color01 = (xen::Color4f)color;
		XEN_CHECK_GL(glClearColor(color01.r, color01.g, color01.b, 1));
		XEN_CHECK_GL(glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT));
	}
}

#endif
