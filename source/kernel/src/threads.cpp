////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Provides platform agnostic implementation of thread related functions,
/// and also includes the platform specific implementations
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_THREADS_CPP
#define XEN_KERNEL_THREADS_CPP

#include "threads.hxx"

// Initially all threads have the id initialised to BAD_THREAD_ID
// When the kernel is initialized/spawns worker threads those threads
// overwrite this value. This ensures that any thread that the kernel
// knows about has a well defined THIS_THREAD_INDEX, but all other
// threads have the BAD_THREAD_ID value
thread_local xen::ThreadIndex xke::THIS_THREAD_INDEX = xen::BAD_THREAD_ID;

xen::ThreadIndex xen::getThreadIndex() {
	return xke::THIS_THREAD_INDEX;
}

#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include "threads.dummy.cpp"
#elif defined XEN_OS_UNIX
	#include "threads.unix.cpp"
#else
	#include "threads.dummy.cpp"
#endif

#endif
