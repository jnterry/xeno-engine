////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains various memory utility functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_UTILITIES_HPP
#define XEN_CORE_MEMORY_UTILITIES_HPP

#include <cstddef>

#include <xen/core/intrinsics.hpp>

namespace xen{
	/// \brief Returns a number of kilobytes as a number of bytes
	inline constexpr size_t kilobytes(const size_t kb) { return kb            * 1024; }

	/// \brief Returns a number of megabytes as a number of bytes
	inline constexpr size_t megabytes(const size_t mb) { return kilobytes(mb) * 1024; }

	/// \brief Returns a number of gigabytes as a number of bytes
	inline constexpr size_t gigabytes(const size_t gb) { return megabytes(gb) * 1024; }

	/// \brief Copies some array of type T from `from` to `to`. Returns param `to`
	template<typename T>
	inline T* copyArray(const T* const from, T* const to, size_t num_elements){
		const T* source = from;
		T*       dest   = to;
		while(num_elements--) { *dest++ = *source++; }
		return to;
	}

	/// \brief Copies some number of bytes from `from` to `to`. Returns param 'to'
	inline void* copyBytes(const void* const from, const void* const to, size_t num_bytes){
		return copyArray<u8>((u8*)from, (u8*)to, num_bytes);
	}

	/// \brief Clears some number of bytes to 0. Returns param `bytes`
	inline void* clearToZero(void* const bytes, size_t num_bytes){
		u8* cur = (u8*)bytes;
		while(num_bytes--) { *cur++ = 0; }
		return bytes;
	}

	template<typename T>
	inline void* clearToZero(T* object){
		return clearToZero(object, sizeof(T));
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

	/// \brief Moves pointer forward some number of bytes
	/// \warn Modifies the paremeter ptr, IE, behaves as +=
	inline void*       ptrAdvance(      void** ptr, size_t bytes){ return (*ptr = (      void*)(((      u8*)*ptr) + bytes)); }
	inline const void* ptrAdvance(const void** ptr, size_t bytes){ return (*ptr = (const void*)(((const u8*)*ptr) + bytes)); }

	/// \brief Moves pointer forward some number of bytes
	inline       void* ptrGetAdvanced(      void* ptr, u32 bytes){ return (      void*)(((      u8*)ptr) + bytes); }
	inline const void* ptrGetAdvanced(const void* ptr, u32 bytes){ return (const void*)(((const u8*)ptr) + bytes); }

	/// \brief Moves pointer backwards some number of bytes
	/// \warn Modifies the paremeter ptr, IE, behaves as -=
	inline void*       ptrRetreat(      void** ptr, size_t bytes){ return (*ptr = (      void*)(((      u8*)*ptr) - bytes)); }
	inline const void* ptrRetreat(const void** ptr, size_t bytes){ return (*ptr = (const void*)(((const u8*)*ptr) - bytes)); }

	/// \brief Moves pointer backwards some number of bytes
	inline       void* ptrGetRetreated(      void* ptr, u32 bytes){ return (      void*)(((      u8*)ptr) - bytes); }
	inline const void* ptrGetRetreated(const void* ptr, u32 bytes){ return (const void*)(((const u8*)ptr) - bytes); }


	/// \brief Returns the distance in bytes between two pointers
	/// \return The number of bytes that ptrA needs to be advanced by in order for
	/// it to be equal to ptrB, return type depends on platform but is always a
	/// signed type
	inline ptrdiff_t ptrDiff(const void* ptrA, const void* ptrB){
		return ((const uint8_t*)ptrB) - ((const uint8_t*)ptrA);
	}
}

#endif
