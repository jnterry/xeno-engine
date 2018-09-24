////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Delcares the kernel WorkQueue type and associated functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_HPP
#define XEN_KERNEL_WORKQUEUE_HPP

namespace xen {

	struct Kernel;
	struct WorkQueue;

	typedef void (*WorkQueueFunction)(WorkQueue* wq, void* work);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a work queue which may have tasks pushed to it
	///
	/// Note that the kernel is free to begin processing tasks in the queue
	/// as soon as it is created and work is pushed
	///
	/// \param kernel The kernel with which to create the WorkQueue
	/// \param function The function called to process each entry in the queue
	/////////////////////////////////////////////////////////////////////
	WorkQueue* createWorkQueue(Kernel& kernel, WorkQueueFunction function);

	/////////////////////////////////////////////////////////////////////
	/// \brief Pushes work onto an existing kernel WorkQueue
	///
	/// \param queue The queue upon which to push work
	/// \param work  The block of data to be passed as the WorkQueueFunction's
	/// work parameter
	/////////////////////////////////////////////////////////////////////
	void pushWork(WorkQueue* queue, void* work);

	/////////////////////////////////////////////////////////////////////
	/// \brief Causes the calling thread to process entries in the specified
	/// work queue until the queue is empty AND every other thread working on
	/// the queue has finished. This ensures that if the WorkQueueFunction
	/// calls pushWork this function will not return until all work entries,
	/// and the entries that they themselves spawn, are completed
	/////////////////////////////////////////////////////////////////////
	void processUntilEmpty(WorkQueue* queue);

	/////////////////////////////////////////////////////////////////////
	/// \brief Frees kernel resources regarding some work queue
	/////////////////////////////////////////////////////////////////////
	void destroyWorkQueue(WorkQueue* queue);
}

#endif
