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

	/// \brief Processing for thread subsystem to be called just before
	/// the start of a tick
	void preTickThreadSubsystem(xen::Kernel* kernel);

	/// \brief Processing for thread subsystem to be called just after
	/// the end of a tick -> this should not return until all TickWork is completed
	void postTickThreadSubsystem(xen::Kernel* kernel);
}

#include <xen/config.hpp>
#include <xen/kernel/TickWork.hpp>

#ifdef XEN_OS_WINDOWS
	#include "TickWork.dummy.hxx"
#elif defined XEN_OS_UNIX
	#include "TickWork.unix.hxx"
#else
	#include "TickWork.dummy.hxx"
#endif

#endif
