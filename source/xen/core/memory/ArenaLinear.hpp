////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains the ArenaLinear type and associated functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ARENA_LINEAR_HPP
#define XEN_CORE_MEMORY_ARENA_LINEAR_HPP

#include <xen/core/memory/utilities.hpp>

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
			: start(block_start), end(ptrGetAdvanced(block_start, block_size)), next_byte(block_start) {
			// no-op
		}
		ArenaLinear (ArenaLinear&& other) : start(other.start), end(other.end), next_byte(other.next_byte) {}
		void* start;     /// \brief First byte of the memory block managed by this arena
		void* end;       /// \brief Last  byte of the memory block managed by this arena
		void* next_byte; /// \brief Next free byte in the memory block
	};

	/// \brief Helper class which will rollback changes to an arena linear when it leaves scope,
	/// unless .commit() is called
	class MemoryTransaction{
	public:
		/// \brief Creates transaction for the specified arena
		inline MemoryTransaction(ArenaLinear& arena)
			: arena(arena), initial_next_byte(arena.next_byte), committed(false) { /* no-op */ }

		// \brief Destructor, rollsback the arena linear unless commit has been called
		inline ~MemoryTransaction(){ if(!committed) { rollback(); } }

		/// \brief Prevents the arena from being rolled back by this class's destructor
		inline void commit(){ committed = true; }

		/// \brief Rolls the arena back to its initial state
		inline void rollback(){ arena.next_byte = initial_next_byte; }
	private:
		ArenaLinear& arena;             //The arena in question
		void*        initial_next_byte; // The initial value of _next_byte
		bool         committed;         // Whether this transaction has been committed
	};

	/// \brief creates a new ArenaLinear of the specifed size, using specified allocator
	/// \note returned arena.start must be deallocated at some point
	ArenaLinear createArenaLinear(Allocator& alloc, uint size);

	/// \brief Resets specified arena such that all space is usable again.
	/// Existing allocations should no longer be used
	void resetArena(ArenaLinear& arena);

	/// \brief Determines how many bytes are unused in the specified arena
	ptrdiff_t bytesRemaining(const ArenaLinear& arena);

	/// \brief Pushes as much of the specified string as possible, truncating if nessacery.
	/// Resulting string always null terminated
	char* pushString(ArenaLinear& arena, const char* str);

	/// \brief Pushes as much of the specified string as possible, truncating if nessacery.
	/// Null terminator is not included
	char* pushStringNoTerminate(ArenaLinear& arena, const char* str);

	/// \brief Reserves some number of bytes in an Arena and returns pointer to the first, does not initialize the bytes
	/// \public \memberof xen::ArenaLinear
	void* reserveBytes(ArenaLinear& arena, size_t num_bytes, u32 align = alignof(int));

	/// \brief Reserves space for some type in an ArenaLinear, does not initialize the reserved space
	/// \param count The number of instances of the type to reserve space for, puts them in continuous array
	/// \public \memberof xen::ArenaLinear
	template<typename T>
	inline T* reserve(ArenaLinear& arena, u32 count = 1, u32 align = alignof(T)){
		return (T*)reserveBytes(arena, sizeof(T) * count, align);
	}
}

#endif
