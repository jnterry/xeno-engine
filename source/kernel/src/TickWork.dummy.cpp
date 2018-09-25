////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of WorkQueue related functions in a dummy
/// fashion which simply processes all entries on a single thread
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_TICKWORK_DUMMY_CPP
#define XEN_KERNEL_TICKWORK_DUMMY_CPP

#warning "Using dummy kernel TickWork implementation"

#include "TickWork.hxx"
#include <xen/kernel/Kernel.hpp>

xen::TickWorkHandle xen::createTickWorkGroup(xen::Kernel& kernel){
	return 1;
}

xen::TickWorkHandle xen::pushTickWork(xen::Kernel& kernel,
                                      xen::TickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){
	work_func(kernel, group, data);
	return 1;
}
xen::TickWorkHandle xen::pushTickWork(xen::Kernel& kernel,
                                      xen::SimpleTickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){
	work_func(data);
	return 1;
}

void xen::waitForTickWork(xen::Kernel&, xen::TickWorkHandle){
	// no-op -> all work completed synchronously
}

bool xke::initThreadSubsystem(xen::Kernel* kernel){
	return true;
}

bool xke::stopThreadSubsystem(xen::Kernel* kernel){
	return true;
}

void xke::preTickThreadSubsystem(xen::Kernel* kernel){
	// no-op
}

void xke::postTickThreadSubsystem(xen::Kernel* kernel){
	// no-op
}

#endif
