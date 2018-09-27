////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of thread related functions in a dummy
/// fashion which simply processes all entries on a single thread
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_THREADS_DUMMY_CPP
#define XEN_KERNEL_THREADS_DUMMY_CPP

#include "threads.hxx"
#include "Kernel.hxx"
#include <xen/kernel/log.hpp>
#include <xen/kernel/Kernel.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

namespace {
	xen::ArenaLinear thread_scratch;
}

xen::TickWorkHandle xen::createTickWorkGroup(){
	return 1;
}

xen::TickWorkHandle xen::pushTickWork(xen::TickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){
	work_func(group, 10, data);
	return 1;
}
xen::TickWorkHandle xen::pushTickWork(xen::SimpleTickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){
	work_func(data);
	return 1;
}

void xen::waitForTickWork(xen::TickWorkHandle){
	// no-op -> all work completed synchronously
}

bool xke::initThreadSubsystem(){
	XenLogWarn("Using dummy thread subsystem - everything will run within a single thread");

	xke::THIS_THREAD_INDEX = 0;

	if(xke::kernel.settings.thread_scratch_size == 0){
		xke::kernel.settings.thread_scratch_size = xen::megabytes(16);
	}

	thread_scratch = xen::createArenaLinear(*xke::kernel.root_allocator,
	                                        xke::kernel.settings.thread_scratch_size
	                                       );

	return true;
}

bool xke::stopThreadSubsystem(){
	xen::destroyArenaLinear(*xke::kernel.root_allocator,
							thread_scratch
	                       );
	return true;
}

void xke::preTickThreadSubsystem(){
	// no-op
}

xen::ArenaLinear& xen::getThreadScratchSpace(){
	xen::ThreadIndex i = xen::getThreadIndex();
	XenAssert(i == 0, "getTickScratchSpace called from invalid thread");

	return thread_scratch;
}

#endif
