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

namespace xsr {

	struct ModuleRasterize {
		xen::ArenaLinear root_arena;

		xen::ArenaPool<xsr::RasterizerMesh> mesh_pool;
		xen::Allocator*                     mesh_attrib_alloc;
	};

	extern ModuleRasterize* state;
}

#endif
