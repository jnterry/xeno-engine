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
#include <xen/kernel/Kernel.hpp>

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
	xke::THIS_THREAD_INDEX = 0;
	return true;
}

bool xke::stopThreadSubsystem(){
	return true;
}

void xke::preTickThreadSubsystem(){
	// no-op
}

#endif
