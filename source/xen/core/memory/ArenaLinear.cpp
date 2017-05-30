////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file ArenaLinear.cpp
/// \author Jamie Terry
/// \date 2016/05/30
/// \brief Contains defintions of functions declared in ArenaLinear.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ARENALINEAR_CPP
#define XEN_CORE_MEMORY_ARENALINEAR_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/Allocator.hpp>

namespace xen{
	ArenaLinear* createArenaLinear(Allocator& alloc, uint size){
		ArenaLinear* result = (ArenaLinear*)alloc.allocate(size + sizeof(ArenaLinear));
		result->start     = ptrAdvance(result, sizeof(ArenaLinear));
		result->next_byte = result->start;
		result->end       = ptrAdvance(result->start, size-1);
		return result;
	}
}

#endif
