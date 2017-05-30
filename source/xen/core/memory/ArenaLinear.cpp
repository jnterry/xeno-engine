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
	ArenaLinear createArenaLinear(Allocator& alloc, uint size){
		void* block = alloc.allocate(size);
		return ArenaLinear(block, size);
	}

	ptrdiff_t bytesRemaining(const ArenaLinear& arena){
		return ptrDiff(arena.next_byte, arena.end);
	}

	void* reserveBytes(ArenaLinear& arena, size_t num_bytes, u32 align){
		void* result = arena.next_byte;
		ptrAlignForward(&result, align);
		arena.next_byte = ptrGetAdvanced(result, num_bytes);
		XenAssert(arena.next_byte <= arena.end, "ArenaLinear too full to reserve bytes");
		return result;
	}

	char* pushString(ArenaLinear& arena, const char* str){
		char* result = pushStringNoTerminate(arena, str);
		if(arena.next_byte < arena.end){
			ptrAdvance(&arena.next_byte, 1);
		}
		*((char*)arena.next_byte) = '\0';
		return result;
	}

	char* pushStringNoTerminate(ArenaLinear& arena, const char* str){
		char* result = (char*)arena.next_byte;
		char* dest   = result;
		while(*str && (dest < arena.end)){
			*dest = *str;
			++dest;
			++str;
		}
		arena.next_byte = dest;
		return result;
	}
}

#endif
