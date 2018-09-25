////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Includes platform specific implementation of TickWork related
/// functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_TICKWORK_CPP
#define XEN_KERNEL_TICKWORK_CPP

#include "TickWork.hxx"

// All threads have the id initialised to BAD_THREAD_ID, this is
// changed when the kernel threads are created such that they
// have the correct values, but any other threads remain
// with the BAD_THREAD_ID
thread_local uint xke::THIS_THREAD_INDEX = ~0;

uint xen::getThreadId() {
	return xke::THIS_THREAD_INDEX;
}

#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include "TickWork.dummy.cpp"
#elif defined XEN_OS_UNIX
	#include "TickWork.unix.cpp"
#else
	#include "TickWork.dummy.cpp"
#endif

#endif
