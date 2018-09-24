////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_HXX
#define XEN_KERNEL_WORKQUEUE_HXX

#include <xen/kernel/TickWork.hpp>

namespace xke {
	bool initThreadSubsystem(xen::Kernel* kernel);
	bool stopThreadSubsystem(xen::Kernel* kernel);
}

#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include "TickWork.dummy.hxx"
#elif defined XEN_OS_UNIX
	#include "TickWork.dummy.hxx"
#else
	#include "TickWork.dummy.hxx"
#endif

#endif
