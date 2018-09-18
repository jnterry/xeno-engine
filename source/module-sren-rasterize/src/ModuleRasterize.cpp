////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of exported rasterizer kernel module
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP
#define XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP

#include "ModuleRasterize.hxx"
#include <xen/sren/rasterizer3d.hxx>
#include <xen/kernel/Kernel.hpp>
#include <xen/core/memory/utilities.hpp>

#include <new>

namespace xen {
	struct Kernel;
}

xsr::ModuleRasterize* xsr::state = nullptr;

void doRasterizerStateInit(void* block, const u64 BLK_SIZE){
	xsr::state = (xsr::ModuleRasterize*)block;

	xsr::state = (xsr::ModuleRasterize*)block;
	xen::clearToZero(xsr::state);

	xsr::state->root_arena.start     = xen::ptrGetAdvanced(block, sizeof(xsr::ModuleRasterize));
	xsr::state->root_arena.end       = xen::ptrGetAdvanced(block, BLK_SIZE);
	xsr::state->root_arena.next_byte = xsr::state->root_arena.start;

	xsr::state->mesh_pool         = xen::createArenaPool<xsr::RasterizerMesh >(xsr::state->root_arena, 128);
	xsr::state->mesh_attrib_alloc = xen::emplace<xen::AllocatorMalloc>(xsr::state->root_arena);
}

namespace {
	void* init(xen::Kernel& kernel){
		const u64 BLK_SIZE = xen::megabytes(4);

		void* data = xen::allocate(kernel, BLK_SIZE, alignof(xsr::ModuleRasterize));
		if(data == nullptr){ return nullptr; }

		doRasterizerStateInit(data, BLK_SIZE);

		return data;
	}

	void shutdown(xen::Kernel& kernel){
		// :TODO: deallocate everything else, eg, mesh data
		xen::deallocate(kernel, xsr::state);
	}
}

#endif
