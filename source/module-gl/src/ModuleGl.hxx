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
#include "Material.hxx"
#include "Texture.hxx"

#include <xen/graphics/ModuleApiGraphics.hpp>
#include <xen/core/memory/ArenaPool.hpp>

namespace xgl {
	struct RenderTargetImpl;

	struct GlState {
		/// \brief The public Api exposed to other modules
		xen::ModuleApiGraphics api;

		/// \brief The primary memory arena used for module wide allocations
		xen::ArenaLinear primary_arena;

		// :TODO: don't store pointers in these pools -> this is legacy
		// code from when we didn't have ArenaPool and looped over an
		// array checking for nullptr
		xen::ArenaPool<xgl::MeshGlData>        pool_mesh;
		xen::ArenaPool<xgl::TextureImpl>       pool_texture;
		xen::ArenaPool<xgl::ShaderProgram>     pool_shader;
		xen::ArenaPool<xgl::Material>          pool_material;
		xen::ArenaPool<xgl::RenderTargetImpl*> pool_render_target;

		/// \brief Default rendering material to be used if render command
		/// specifies nullptr for material
		const xgl::Material* default_material;
	};

	extern GlState* gl_state;
}


#endif
