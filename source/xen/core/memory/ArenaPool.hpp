////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for a pool memory arena
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_ARENAPOOL_HPP
#define XEN_CORE_ARENAPOOL_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	class Allocator;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a fixed size pool of items of some type
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct ArenaPool {
		/// \brief Represents a single slot in the pool
		union Slot {
			/// \brief The object of type T stored in this slot
			T  item;

			/// \brief Index of the next free slot
			u32 next_free;
		};

		/// \brief Array of slots that may be used by this ArenaPool
		Slot* slots;

		/// \brief Total number of slots in this ArenaPool
		u32 capacity;

		/// \brief Number of slots currently in use
		u32 slots_used;

		/// \brief Index of the first_free_slot. This acts as a head
		/// of a singly linked list - subsequent elements can be found
		/// using the "next_free" member of the slot
		/// Note that we use array indices rather than pointers to save memory
		/// if sizeof(T) < sizeof(Slot*)
		u32 first_free_slot;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a (potentially) resizable pool of of items of
	/// some type
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct ArenaPool {
		static_assert(sizeof(T) >= sizeof(u32));



		/// \brief Represents a block of pool Slot instances
		struct Block {
			/// \brief Pointer to the array of slots in this Block
			PoolSlot*  slots;

			/// \brief Index of the first free slot in this block
			u32        first_free;

			/// \brief The number of slots
			u32        capacity;
			u32        slots_used;
			PoolBlock* next_block;
		};

		Allocator* backing_allocator;
		PoolBlock* first_block;
		u32        capacity;
		u32        slots_used;
		PoolBlock* first_block;
	};

	template<typename T>
	ArenaPool createArenaPool(Allocator* allocator, u32 capacity);

	/////////////////////////////////////////////////////////////////////
	/// \brief Deallocates all memory associated with the specified ArenaPool
	///
	/// This invalidates any items allocated from the pool
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	ArenaPool destroyArenaPool(Allocator* allocator);

	/////////////////////////////////////////////////////////////////////
  /// \brief Reserves a slot from the specified ArenaPool for an instance of
	/// type T
	/// \public \memberof xen::ArenaPool
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	T* reserveType(ArenaPool<T> arena){

	  return (T*)reserveBytes(arena, sizeof(T), alignof(T));
	}

	/// \brief Frees
	template<typename T>
	T* freeType(ArenaPool<T>& arena) {

	}

	/// \brief Pushes a new instance of some class into some arena. After
	/// allocation calls the class's constructor with specified arguments
	template<typename T, typename... T_ARGS>
	inline T* emplace(ArenaLinear& arena, T_ARGS... args){
		return new (reserveType<T>(arena)) (T)(args...);
	}



}

#endif
