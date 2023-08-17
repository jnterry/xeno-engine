////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains definitions of types and functions declared in Allocator.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ALLOCATOR_CPP
#define XEN_CORE_MEMORY_ALLOCATOR_CPP

#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/utilities.hpp>
#include <cstdlib>

namespace xen {
	void* AllocatorMalloc::allocate(u32 size, u32 align){
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
		void* result = malloc(size + align);
		return ptrGetAlignedForward(result, align);
#pragma GCC diagnostic pop
	}

	void  AllocatorMalloc::deallocate(void* block){
		free(block);
	}
}

#endif
