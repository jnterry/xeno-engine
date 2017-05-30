////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file ArenaLinear.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains the ArenaLinear type and associated functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ARENA_LINEAR_HPP
#define XEN_CORE_MEMORY_ARENA_LINEAR_HPP

#include "utilities.hpp"

/// \brief Creates a tempory ArenaLinear by allocating space from the stack
/// \param name Name of arena to create
#define XenTempArena(name, size)	  \
	char _xen_temp_arena_buf_ ## name[size]; \
	::xen::ArenaLinear name(_xen_temp_arena_buf_ ## name, size);

namespace xen{
	class Allocator;

	/// \brief Represents a memory arena which allocates memory in order from start to end,
	/// cannot free individual allocations, but can rollback to some other point
	struct ArenaLinear : public NonCopyable{
		ArenaLinear(void* block_start, size_t block_size)
			: start(block_start), end(ptrAdvance(block_start, block_size)), next_byte(block_start) {
			// no-op
		}
		void* start;     /// \brief First byte of the memory block managed by this arena
		void* end;       /// \brief Last  byte of the memory block managed by this arena
		void* next_byte; /// \brief Next free byte in the memory block
	};

	/// \brief Helper class which will rollback changes to an arena linear when it leaves scope,
	/// unless .commit() is called
	class MemoryTransaction{
	public:
		/// \brief Creates transaction for the specified arena
		inline MemoryTransaction(ArenaLinear* arena)
			: arena(arena), initial_next_byte(arena->next_byte), committed(false) { /* no-op */ }

		// \brief Destructor, rollsback the arena linear unless commit has been called
		inline ~MemoryTransaction(){ if(!committed) { rollback(); } }

		/// \brief Prevents the arena from being rolled back by this class's destructor
		inline void commit(){ committed = true; }

		/// \brief Rolls the arena back to its initial state
		inline void rollback(){ arena->next_byte = initial_next_byte; }
	private:
		ArenaLinear* arena;             //The arena in question
		void*        initial_next_byte; // The initial value of _next_byte
		bool         committed;         // Whether this transaction has been committed
	};

	/// \brief creates a new ArenaLinear of the specifed size, using specified allocator
	ArenaLinear* createArenaLinear(Allocator& alloc, uint size);
}

#endif
