////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file memory.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains various helpers for dealing with memory allocation
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_HPP
#define XEN_CORE_MEMORY_HPP

#include "intrinsics.hpp"

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

	/// \brief Represents a memory arena which allocates memory in order from start to end,
	/// cannot free individual allocations, but can rollback to some other point
	struct ArenaLinear : public NonCopyable{
		void* _start;
		void* _end;
		void* _next_byte;
	};

	/// \brief Helper class which will rollback changes to an arena linear when it leaves scope,
	/// unless .commit() is called
	class MemoryTransaction{
	public:
		/// \brief Creates transaction for the specified arena
		inline MemoryTransaction(ArenaLinear* arena)
			: arena(arena), initial_next_byte(arena._next_byte), commited(false) { /* no-op */ }

		// \brief Destructor, rollsback the arena linear unless commit has been called
		inline ~MemoryTransaction(){ if(!committed) { rollback(); } }

		/// \brief Prevents the arena from being rolled back by this class's destructor
		inline void commit(){ committed = true; }

		/// \brief Rolls the arena back to its initial state
		inline void rollback(){ arena->_next_byte = initial_next_byte; }
	private:
		ArenaLinear* arena;             //The arena in question
		void*        initial_next_byte; // The initial value of _next_byte
		bool         committed;         // Whether this transaction has been committed
	};
}

#endif
