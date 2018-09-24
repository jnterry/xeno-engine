////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of WorkQueue related functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_CPP
#define XEN_KERNEL_WORKQUEUE_CPP

#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include "WorkQueue.dummy.cpp"
#elif defined XEN_OS_UNIX
	#include "WorkQueue.dummy.cpp"
#else
	#include "WorkQueue.dummy.cpp"
#endif

#endif
