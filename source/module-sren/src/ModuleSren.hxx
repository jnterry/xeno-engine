////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains base global data type for software rendering modules
///
/// \ingroup module-sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_MODULEBASE_HXX
#define XEN_SREN_MODULEBASE_HXX

#include <xen/core/memory/ArenaPool.hpp>
#include "RenderTargetImpl.hxx"

namespace xsren {
	struct SrenStateBase {
		xen::ArenaPool<xsren::RenderTarget> pool_render_target;
	};

	// :TODO: temp debug function so sren_state can be initialized by the graphics
	// devices. Once we remove those this can go
	initSrenStateBase(void* mem_block, u64 mem_block_size);

	SrenStateBase* sren_state;
}

#endif
