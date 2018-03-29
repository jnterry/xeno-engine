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
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/utilities.hpp>

namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a fixed size pool of items of some type
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct ArenaPool : public xen::NonCopyable {
		/// \brief Value used for Slot::next_free to indicate
		/// there is no next free slot
		static const constexpr u32 BAD_SLOT_INDEX = 0xFFFFFFFF;

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

		ArenaPool(){
			// no-op
		}

		ArenaPool (ArenaPool&& other) :
			slots(other.slots), capacity(other.capacity),
			slots_used(other.slots_used) {
			// no-op
		}
	};

	template<typename T>
	ArenaPool<T> createArenaPool(Allocator* allocator, u32 capacity){
		ArenaPool<T> result;
		xen::clearToZero(&result);

		////////////////////////////////////////////////////////////////////
		// Allocate storage
		result.slots = (typename ArenaPool<T>::Slot*)
			allocator->allocate(sizeof(T) * capacity, alignof(T));

		if(result.slots == nullptr){
			return result;
		}

		////////////////////////////////////////////////////////////////////
		// Initialise members
		result.capacity        = capacity;
		result.slots_used      = 0;
		result.first_free_slot = 0;

		////////////////////////////////////////////////////////////////////
		// Initialise the slot linked list to have each point at the next
		for(u32 i = 0; i < capacity-1; ++i){
			result.slots[i].next_free = i + 1;
		}
		result.slots[capacity-1].next_free = ArenaPool<T>::BAD_SLOT_INDEX;

		return result;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Deallocates all memory associated with the specified ArenaPool
	///
	/// This invalidates any items allocated from the pool
	/////////////////////////////////////////////////////////////////////
	template<typename T>
  void destroyArenaPool(Allocator* allocator, ArenaPool<T>& pool){
		allocator->deallocate(pool.slots);
	}

	/////////////////////////////////////////////////////////////////////
  /// \brief Reserves a slot from the specified ArenaPool for an instance of
	/// type T
	/// \public \memberof xen::ArenaPool
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	T* reserveType(ArenaPool<T>& arena){
		if(arena.first_free_slot == ArenaPool<T>::BAD_SLOT_INDEX){
			return nullptr;
		}
		++arena.slots_used;
		typename ArenaPool<T>::Slot* slot = &arena.slots[arena.first_free_slot];
		arena.first_free_slot = slot->next_free;
		return &slot->item;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Frees an instance of some type, allowing the slot in the specified
	/// pool arena to be reused in a future allocation
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	void freeType(ArenaPool<T>& arena, T* object) {
		u32 slot_index = (object - &arena.slots[0].item) / sizeof(T);
		--arena.slots_used;
		arena.slots[slot_index].next_free = arena.first_free_slot;
		arena.first_free_slot = slot_index;
	}

	/// \brief Pushes a new instance of some class into some arena. After
	/// allocation calls the class's constructor with specified arguments
	template<typename T, typename... T_ARGS>
	inline T* emplace(ArenaPool<T>& arena, T_ARGS... args){
		return new (reserveType<T>(arena)) (T)(args...);
	}
}

#endif
