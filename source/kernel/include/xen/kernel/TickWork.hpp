////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Declares interface for submitting tick bounded work to the kernel
/// to be performed asynchronously from the main thread
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_TICKWORK_HPP
#define XEN_KERNEL_TICKWORK_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	struct Kernel;

	/// \brief Handle to some piece of tick work being performed by the kernel
	typedef unsigned int TickWorkHandle;

	/////////////////////////////////////////////////////////////////////
	/// \brief Function representing work to be performed asynchronously by the
	/// end of the tick. Passed the kernel and parent group such that the function
	/// can enqueue more work into the same group, eg, for a divide and conquer
	/// algorithm where each division of the problem can be further ran in parallel
	/////////////////////////////////////////////////////////////////////
	typedef void (*TickWorkFunction)(xen::Kernel& kernel, TickWorkHandle parent, void* data);

	/////////////////////////////////////////////////////////////////////
	/// \brief Simpler function signature than TickWorkFunction for representing
	/// work to be performed asynchronously by the end of the tick
	/////////////////////////////////////////////////////////////////////
	typedef void (*SimpleTickWorkFunction)(void* data);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a handle by which future tick work can be grouped,
	/// thus allowing waitForWork to wait until multiple pieces of tick work
	/// are completed
	/////////////////////////////////////////////////////////////////////
	TickWorkHandle createTickWorkGroup(Kernel& kernel);

	/////////////////////////////////////////////////////////////////////
	/// \brief Pushes work which must be completed before the end of the
	/// tick at the latest or at some other point during the tick by calling
	/// waitForTickWork with the TickWorkHandle returned by this function
	///
	/// \param kernel    The kernel in which to create the work
	/// \param work_func The function to be called to complete the work
	/// \param data      Data to be passed to the TickWorkFunction
	/// \param data_size The size of the memory block pointed to be data
	/// \param group  The work group (as returned by createTickWorkGroup)
	/// that this work should be a part of
	///
	/// \note A byte-wise shallow copy of data will be made such that it may
	/// be passed to the TickWorkFunction. This means that the address cannot be
	/// relied upon to remain constant. Additionally the data should be kept
	/// small and instead refer to other data blocks by pointer - however care
	/// must be taken by the caller to ensure these external data blocks are
	/// valid until the end of the tick (or until waitForTickWork has returned
	/// for this particular piece of work)
	/////////////////////////////////////////////////////////////////////
	TickWorkHandle pushTickWork(Kernel& kernel,
	                            TickWorkFunction work_func,
	                            void* data, u64 data_size,
	                            TickWorkHandle group = 0
	                           );
	TickWorkHandle pushTickWork(Kernel& kernel,
	                            SimpleTickWorkFunction work_func,
	                            void* data, u64 data_size,
	                            TickWorkHandle group = 0
	                           );
	template<typename T_FUNC, typename T_DATA>
	inline TickWorkHandle pushTickWork(Kernel& kernel,
	                                   T_FUNC work_func,
	                                   T_DATA* data,
	                                   TickWorkHandle group = 0){
		return pushTickWork(kernel, work_func, data, sizeof(T_DATA), group);
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Waits for some tick work, or work group, to be completed
	/////////////////////////////////////////////////////////////////////
	void waitForTickWork(TickWorkHandle work);
}

#endif
