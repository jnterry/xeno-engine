////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation details for kernel threading system
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_HXX
#define XEN_KERNEL_WORKQUEUE_HXX

#include <xen/core/intrinsics.hpp>
#include <xen/kernel/threads.hpp>

namespace xke {
	/////////////////////////////////////////////////////////////////////
	/// \brief Performs platform specific initialization of the kernel
	/// thread subsystem, will be called as part of initKernel
	/////////////////////////////////////////////////////////////////////
	bool initThreadSubsystem(xen::Kernel* kernel);

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs platform specific shutdown of the kernel thread system,
	/// will be called as part of kernel shutdown procedure
	/////////////////////////////////////////////////////////////////////
	bool stopThreadSubsystem(xen::Kernel* kernel);

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs platform specific behaviour just before a tick begins
	/////////////////////////////////////////////////////////////////////
	void preTickThreadSubsystem(xen::Kernel* kernel);

	extern thread_local uint THIS_THREAD_INDEX;
}

#include <xen/config.hpp>
#include <xen/kernel/threads.hpp>

#ifdef XEN_OS_WINDOWS
	#include "threads.dummy.hxx"
#elif defined XEN_OS_UNIX
	#include "threads.unix.hxx"
#else
	#include "threads.dummy.hxx"
#endif

#endif
