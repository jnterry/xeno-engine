////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of WorkQueue related functions in a dummy
/// fashion which simply processes all entries on a single thread
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_DUMMY_CPP
#define XEN_KERNEL_WORKQUEUE_DUMMY_CPP

#warning "Using kernel work queue dummy implementation"

#include <xen/kernel/WorkQueue.hpp>
#include "WorkQueue.hxx"
#include <xen/kernel/Kernel.hpp>

struct xen::WorkQueue {
	xen::Kernel* owner;
	xen::WorkQueueFunction function;
};

xen::WorkQueue* xen::createWorkQueue(Kernel& kernel, xen::WorkQueueFunction function){
	xen::WorkQueue* queue = (xen::WorkQueue*)xen::allocate(kernel,
	                                                       sizeof(xen::WorkQueue),
	                                                       alignof(xen::WorkQueue)
	                                                      );
	queue->owner    = &kernel;
	queue->function = function;
	return queue;
}

void xen::pushWork(xen::WorkQueue* queue, void* work){
	queue->function(queue, work);
}

void xen::processUntilEmpty(xen::WorkQueue* queue){
	// no-op -> there is never a backlog of work
}

void xen::destroyWorkQueue(xen::WorkQueue* queue){
	xen::deallocate(*queue->owner, queue);
}

void xke::initThreadData(xen::Kernel& kernel){
	// no-op
}

#endif
