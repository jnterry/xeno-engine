////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains defintions of functions declared in ArenaLinear.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ARENALINEAR_CPP
#define XEN_CORE_MEMORY_ARENALINEAR_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/atomic_intrinsics.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/Allocator.hpp>

namespace xen{
	ArenaLinear createArenaLinear(Allocator& alloc, uint size){
		void* block = alloc.allocate(size);
		return ArenaLinear(block, size);
	}

	ArenaLinear createArenaLinear(ArenaLinear& parent, uint size){
		void* block = xen::reserveBytes(parent, size);
		return ArenaLinear(block, size);
	}

  void destroyArenaLinear(Allocator& alloc, ArenaLinear& arena){
		alloc.deallocate(arena.start);
		arena.start     = nullptr;
		arena.end       = nullptr;
		arena.next_byte = nullptr;
	}

	void resetArena(ArenaLinear& arena){
		arena.next_byte = arena.start;
	}

	bool isValid(ArenaLinear& arena){
		return ((uptr)arena.next_byte) <= ((uptr)arena.end+1);
	}

	// The casts to u64 are safe  since we know these differences are
	// positive based on the direction
  u64 getBytesRemaining(const ArenaLinear& arena){
	  return (u64)ptrDiff(arena.next_byte, arena.end);
	}
	u64 getBytesUsed     (const ArenaLinear& arena){
		return (u64)ptrDiff(arena.start, arena.next_byte) - 1;
	}
	u64 getSize          (const ArenaLinear& arena){
		return (u64)ptrDiff(arena.start, arena.end);
	}

	void* pushBytes(ArenaLinear& arena, void* data, size_t num_bytes, u32 align){
		void* result = reserveBytes(arena, num_bytes, align);
		xen::copyBytes(data, result, num_bytes);
		return result;
	}

	void* sync::pushBytes(ArenaLinear& arena, void* data, size_t num_bytes, u32 align){
		void* result = sync::reserveBytes(arena, num_bytes, align);
		xen::copyBytes(data, result, num_bytes);
		return result;
	}

	void* reserveBytes(ArenaLinear& arena, size_t num_bytes, u32 align){
		void* result = arena.next_byte;
		ptrAlignForward(&result, align);
		arena.next_byte = ptrGetAdvanced(result, num_bytes);
		XenAssert(arena.next_byte <= arena.end, "ArenaLinear too full to reserve bytes");
		return result;
	}

	void* sync::reserveBytes(ArenaLinear& arena, size_t num_bytes, u32 align){
		#if 1
		// minimal memory usage, SLOWEST -> 100000 allocs in 4.08ms
		void *cur_next_byte, *new_next_byte, *result;

		do {
			cur_next_byte = arena.next_byte;
			result        = xen::ptrGetAlignedForward(arena.next_byte, align);
			new_next_byte = xen::ptrGetAdvanced (result, num_bytes);
		} while (!xen::sync::compareExchange(&arena.next_byte, cur_next_byte, new_next_byte));


		#elif 0
		// some wasted memory as assume worst case for alignment
		// FASTEST -> 100000 allocs in 3.16ms
		void* result = xen::sync::fetchAndAdd(&arena.next_byte, (void*)(num_bytes + align - 1));
		ptrAlignForward(&result, align);
		#endif

		// for reference:
		// standard xen::pushBytes -> 3.81ms, minimal memory usage
		// malloc                  -> 5.23ms, ??? memory usage

		XenAssert(arena.next_byte <= arena.end, "ArenaLinear too full to reserve bytes");
		return result;
	}

	char* pushString(ArenaLinear& arena, const char* str){
		char* result = pushStringNoTerminate(arena, str);
		if(arena.next_byte < arena.end){
			*((char*)arena.next_byte) = '\0';
			ptrAdvance(&arena.next_byte, 1);
		} else {
			// always null terminate, truncating string if not space
			*((char*)arena.end) = '\0';
		}
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
