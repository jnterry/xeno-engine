////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file memory/utilities.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains various memory utility functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_UTILITIES_HPP
#define XEN_CORE_MEMORY_UTILITIES_HPP

#include <cstddef>

#include "../intrinsics.hpp"

namespace xen{
	/// \brief Returns a number of kilobytes as a number of bytes
	inline constexpr size_t kilobytes(const size_t kb) { return kb            * 1024; }

	/// \brief Returns a number of megabytes as a number of bytes
	inline constexpr size_t megabytes(const size_t mb) { return kilobytes(mb) * 1024; }

	/// \brief Returns a number of gigabytes as a number of bytes
	inline constexpr size_t gigabytes(const size_t gb) { return megabytes(gb) * 1024; }

	/// \brief Copies some number of bytes from A to B, returns param 'to'
	inline void* copyBytes(const void* const from, void* const to, size_t num_bytes){
		u8* source = (u8*)from;
		u8* dest   = (u8*)to;
		while(num_bytes--) { *dest++ = *source++; }
		return to;
	}

	/// \brief Advances the pointer ptr such that its address mod align is 0,
	/// \return Result of the alignment
	inline void* ptrGetAlignedForward(const void* const ptr, size_t align){
		uintptr_t ptr_int = uintptr_t(ptr);
		const uintptr_t mod = ptr_int % align;
		if (mod){ ptr_int += (align - mod); }
		return (void *)ptr_int;
	}

	/// \brief Advances a pointer (if nessaccery) such that its address mod align is 0
	/// \param ptr Pointer to the pointer to align, function modifies this!
	/// \return Number of bytes the pointer was advanced
	inline u32 ptrAlignForward(void** ptr, u32 align){
		uintptr_t ptr_int = uintptr_t(*ptr);
		u32 delta = ptr_int % align;
		if(delta) { delta = align - delta; }
		*ptr = (void*)(ptr_int + delta);
		return delta;
	}

	/// \brief Increases the value of a pointer by some number of bytes, useful to
	/// ensure it is moved by the number of bytes rather than by a multiple of the
	/// size of the pointed at object
	inline void* ptrAdvance(void* ptr, size_t bytes){
		return (void*)(((uint8_t*)ptr) + bytes);
	}
	/// \overload
	inline const void* ptrAdvance(const void* ptr, size_t bytes){
		return (const void*)(((const uint8_t*)ptr) + bytes);
	}

	/// \brief Decreases the value of a pointer by some number of bytes, useful to
	/// ensure it is moved by the number of bytes rather than by a multiple of the
	/// size of the pointed at object
	inline void* ptrRetreat(void* ptr, size_t bytes){
		return (void*)(((uint8_t*)ptr) - bytes);
	}
	/// \overload
	inline const void* ptrRetreat(const void* ptr, size_t bytes){
		return (const void*)(((const uint8_t*)ptr) - bytes);
	}

	/// \brief Returns the distance in bytes between two pointers
	/// \return The number of bytes that ptrA needs to be advanced by in order for
	/// it to be equal to ptrB, return type depends on platform but is always a
	/// signed type
	inline ptrdiff_t ptrDiff(const void* ptrA, const void* ptrB){
		return ((const uint8_t*)ptrB) - ((const uint8_t*)ptrA);
	}
}

#endif
