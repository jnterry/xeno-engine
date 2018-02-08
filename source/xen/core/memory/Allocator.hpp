////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains Allocator types
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_MEMORY_ALLOCATOR_HPP
#define XEN_CORE_MEMORY_ALLOCATOR_HPP

#include "../intrinsics.hpp"

namespace xen{
	//////////////////////////////////////////////////////////////////////////
	/// \brief Virtual class which represents some allocator from which memory may
	/// be obtained
	/// \note Allocation is slow, Allocator instances are intended to be used
	/// infrequently in order to obtain large blocks of memory from the OS, which
	/// are then managed manually by some top level system in Xenogin
	//////////////////////////////////////////////////////////////////////////
	class Allocator{
	public:
		virtual void* allocate  (u32 size, u32 align = alignof(int)) = 0;
		virtual void  deallocate(void* block) = 0;
		virtual ~Allocator(){};
	};

	//////////////////////////////////////////////////////////////////////////
	/// \brief Allocator which simply wraps malloc/free
	//////////////////////////////////////////////////////////////////////////
	class AllocatorMalloc : public Allocator{
	public:
		AllocatorMalloc (){}
		virtual ~AllocatorMalloc(){}

		void* allocate  (u32 size, u32 align = alignof(int));
		void  deallocate(void* block);
	};

	//////////////////////////////////////////////////////////////////////////
	/// \brief Debug allocator which wraps some other and counts how many allocations have
	/// been made vs freed, hence allowing us to detect memory leaks
	//////////////////////////////////////////////////////////////////////////
	template <typename T>
	class AllocatorCounter : public T, NonCopyable{
	public:
		AllocatorCounter() : alloc_count(0) {
			//no-op
		}

		~AllocatorCounter() {
			XenAssert(alloc_count == 0, "Outstanding allocations on allocator destruction!");
		}

		void* allocate(u32 size, u32 align = alignof(int)){
			void* result = T::allocate(size, align);
			if(result !=  nullptr){ ++alloc_count; }
			return result;
		}

		void deallocate(void* block){
			--alloc_count;
			T::deallocate(block);
		}
	private:
		int alloc_count;
	};
}

#endif
