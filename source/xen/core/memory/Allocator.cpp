////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains definitions of types and functions declared in Allocator.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ALLOCATOR_CPP
#define XEN_CORE_MEMORY_ALLOCATOR_CPP

#include "Allocator.hpp"
#include "utilities.hpp"
#include <cstdlib>

namespace xen{
	void* AllocatorMalloc::allocate  (u32 size, u32 align){
		void* result = malloc(size + align);
		return ptrGetAlignedForward(result, align);
	}

	void  AllocatorMalloc::deallocate(void* block){
		free(block);
	}
}

#endif
