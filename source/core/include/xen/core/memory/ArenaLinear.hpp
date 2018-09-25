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

#include <new>

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
		ArenaLinear()
			: start(nullptr), end(nullptr), next_byte(nullptr){
			// no-op
		}

		ArenaLinear(void* block_start, size_t block_size)
			: start(block_start), end(ptrGetAdvanced(block_start, block_size)), next_byte(block_start) {
			// no-op
		}

		inline ArenaLinear (ArenaLinear&& other) :
			start(other.start), end(other.end), next_byte(other.next_byte) {
			// no-op
		}

		inline ArenaLinear& operator=(ArenaLinear&& other){
			this->start           = other.start;
			this->end             = other.end;
			this->next_byte       = other.next_byte;

			xen::clearToZero(&other);

			return *this;
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

	/// \brief creates a new ArenaLinear of the specified size, reserving
	/// storage space from some other ArenaLinear
	ArenaLinear createArenaLinear(ArenaLinear& parent, uint size);

	/// \brief Destroys an ArenaLinear previously created with createArenaLinear
	void destroyArenaLinear(Allocator& alloc, ArenaLinear& arena);

	/// \brief Resets specified arena such that all space is usable again.
	/// Existing allocations should no longer be used
	void resetArena(ArenaLinear& arena);

	bool isValid(ArenaLinear& arena);

	/// \brief Determines how many bytes are unused in the specified arena
	u64 getBytesRemaining(const ArenaLinear& arena);

	/// \brief Determines how many bytes have been used in the specified arena
	u64 getBytesUsed(const ArenaLinear& arena);

	/// \brief Determines the total size (in bytes) of the specified arena
	u64 getSize(const ArenaLinear& arena);

	/// \brief Pushes as much of the specified string as possible, truncating if nessacery.
	/// Resulting string always null terminated
	char* pushString(ArenaLinear& arena, const char* str);

	/// \brief Pushes as much of the specified string as possible, truncating if nessacery.
	/// Null terminator is not included
	char* pushStringNoTerminate(ArenaLinear& arena, const char* str);

	/// \brief Reserves bytes and then memcpy's data into them
	/// \public \memberof xen::ArenaLinear
	void* pushBytes(ArenaLinear& arena, void* data, size_t num_bytes, u32 align = alignof(int));

	/// \brief Reserves some number of bytes in an Arena and returns pointer to
	/// the first. Does not initialise the bytes
	/// \public \memberof xen::ArenaLinear
	void* reserveBytes(ArenaLinear& arena, size_t num_bytes, u32 align = alignof(int));

	/// \brief Reserves space for an array of some type in an ArenaLinear,
	/// does not initialise the reserved space
	/// \public \memberof xen::ArenaLinear
	template<typename T>
	T* reserveTypeArray(ArenaLinear& arena, u32 length){
		return (T*)reserveBytes(arena, sizeof(T) * length, alignof(T));
	}

	/// \brief Reserves space for some type in an ArenaLinear,
	/// does not initialise the reserved space
	/// \public \memberof xen::ArenaLinear
	template<typename T>
	T* reserveType(ArenaLinear& arena){
	  return (T*)reserveBytes(arena, sizeof(T), alignof(T));
	}

	/// \brief Pushes a new instance of some class into some arena. After
	/// allocation calls the class's constructor with specified arguments
	template<typename T, typename... T_ARGS>
	inline T* emplace(ArenaLinear& arena, T_ARGS... args){
		return new (reserveType<T>(arena)) (T)(args...);
	}

	namespace sync {
		void* reserveBytes(ArenaLinear& arena, size_t num_bytes, u32 align = alignof(int));
		void* pushBytes   (ArenaLinear& arena, void* data, size_t num_bytes, u32 align = alignof(int));

		template<typename T>
		T* reserveTypeArray(ArenaLinear& arena, u32 length){
			return (T*)sync::reserveBytes(arena, sizeof(T) * length, alignof(T));
		}

		template<typename T>
		T* reserveType(ArenaLinear& arena){
			return (T*)sync::reserveBytes(arena, sizeof(T), alignof(T));
		}

		template<typename T, typename... T_ARGS>
		inline T* emplace(ArenaLinear& arena, T_ARGS... args){
			return new (sync::reserveType<T>(arena)) (T)(args...);
		}
	}
}

#endif
