////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains module wide type declerations
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MODULE_HXX
#define XEN_MODULESRENRASTERIZE_MODULE_HXX

#include <xen/core/memory/ArenaPool.hpp>
#include <xen/sren/rasterizer3d.hxx>
#include <xen/sren/RenderTarget.hxx>
#include <xen/graphics/GraphicsModuleApi.hpp>

#include "Texture.hxx"

#include <thpool.h>

namespace xsr {

	struct PostProcessor;

	struct ModuleRasterize {
		xen::ArenaLinear root_arena;

		xen::ArenaPool<xsr::RasterizerMesh> mesh_pool;
		xen::Allocator*                     mesh_attrib_alloc;

		xen::ArenaPool<xsr::Texture>        texture_pool;
		xen::Allocator*                     texture_pixel_alloc;

		xen::ArenaPool<xsr::FragmentShader> shader_pool;

		xen::Array<xsr::PostProcessor*>     post_processors;

		xen::ArenaPool<xsr::RenderTarget>   render_target_pool;
		xen::Allocator*                     render_target_alloc;

		/// \brief List of operations to be performed next tick
		xen::FixedArray<xen::RenderOp, 128> op_list;
		/// \brief index of the next operation in op_list which has not yet been used
		u64 next_free_op;

		threadpool thpool;

		xen::GraphicsModuleApi api;
	};

	extern ModuleRasterize* state;
}

#endif
