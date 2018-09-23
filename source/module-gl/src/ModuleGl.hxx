////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains global types for module-gl
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_MODULE_HXX
#define XEN_GL_MODULE_HXX

#include "Mesh.hxx"
#include "Shader.hxx"
#include "Texture.hxx"

#include <xen/graphics/GraphicsModuleApi.hpp>
#include <xen/core/memory/ArenaPool.hpp>

namespace xen {
	namespace gl {
		struct RenderTargetImpl;
	}
}

namespace xgl {

	struct GlState {
		/// \brief The public Api exposed to other modules
		xen::GraphicsModuleApi api;

		/// \brief The primary memory arena used for module wide allocations
		xen::ArenaLinear primary_arena;

		// :TODO: don't store pointers in these pools -> this is legacy
		// code from when we didn't have ArenaPool and looped over an
		// array checking for nullptr
		xen::ArenaPool<xgl::MeshGlData>        pool_mesh;
		xen::ArenaPool<xgl::TextureImpl>       pool_texture;
		xen::ArenaPool<xen::gl::RenderTargetImpl*> pool_render_target;

		xgl::ShaderProgram* default_shader;
	};

	extern GlState* gl_state;
}


#endif
