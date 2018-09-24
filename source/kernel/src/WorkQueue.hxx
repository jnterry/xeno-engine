////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_HXX
#define XEN_KERNEL_WORKQUEUE_HXX

#include <xen/kernel/WorkQueue.hpp>

namespace xke {
	void initThreadData(xen::Kernel& kernel);
}

#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include "WorkQueue.dummy.hxx"
#elif defined XEN_OS_UNIX
	#include "WorkQueue.dummy.hxx"
#else
	#include "WorkQueue.dummy.hxx"
#endif

#endif
