////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Declares interface to the kernel for various thread related activity,
/// for example, submitting tick bounded work to the kernel that should be
/// performed asynchronously from the main thread
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_THREADS_HPP
#define XEN_KERNEL_THREADS_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	struct Kernel;

	/////////////////////////////////////////////////////////////////////
	/// \brief Handle to some piece of work which must be completed by the end
	/// of the tick on which it was submitted. TickWorkHandle's should not be
	/// persisted across ticks, since they will be reused on each tick
	/////////////////////////////////////////////////////////////////////
	typedef unsigned int TickWorkHandle;

	/////////////////////////////////////////////////////////////////////
	/// \brief Function representing work to be performed asynchronously by the
	/// end of the tick.
	///
	/// \param kernel The kernel performing the work
	/// \param id     The TickWorkHandle this work is for - this allows the
	/// function to submit more work to the kernel as a sub-task of this, hence
	/// ensuring that any thread waiting on this work to complete will also wait
	/// for the tasks spawned by this to be completed. This is particularly useful
	/// for divide and conquer algorithms where some task is split into n pieces
	/// that can be ran in parallel, each of which can be split again and run in
	/// parallel. By waiting on the root task a thread can ensure all sub-splits
	/// are also completed by the time waitForTickWork completes
	/////////////////////////////////////////////////////////////////////
	typedef void (*TickWorkFunction)(xen::Kernel& kernel, TickWorkHandle id, void* data);

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
	/// \brief Waits for some tick work to be completed. This is defined as
	/// the work itself being finished, as well as any sub-tasks spawned by
	/// pushTickWork where group is equal to the work handle passed to this
	/// function. Note that the calling thread will help out by processing
	/// any outstanding tasks
	/////////////////////////////////////////////////////////////////////
	void waitForTickWork(Kernel& kernel, TickWorkHandle work);

	/////////////////////////////////////////////////////////////////////
	/// \brief Unsigned type representing the index of some kernel thread
	///
	/// Id 0 is reserved for the thread which created the kernel, all other
	/// kernel threads will have sequential integers starting from 1 up to
	/// (kernel_settings.thread_count - 1)
	/////////////////////////////////////////////////////////////////////
	typedef uint ThreadIndex;

	/////////////////////////////////////////////////////////////////////
	/// \brief ThreadIndex representing a bad thread, this is one that was
	/// created outside of the kernel
	/////////////////////////////////////////////////////////////////////
	constexpr const ThreadIndex BAD_THREAD_ID = ~0;

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the id of the calling thread according to the running
	/// kernel, id 0 represents the main thread which created the kernel, and
	/// which all module tick functions will be called under. Remaining id's
	/// are sequential integers from 1 to kernel_settings.thread_count-1
	///
	/// \note BAD_THREAD_ID will be returned if the calling thread is neither
	/// the main thread which created the kernel, nor a worker thread spawned
	/// by the kernel
	/////////////////////////////////////////////////////////////////////
	uint getThreadId();
}

#endif
