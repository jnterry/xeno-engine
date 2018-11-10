////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains various wrappers around compiler specific intrinsics for
/// performing atomic manipulations of variables
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_ATOMICINTRINSICS_HPP
#define XEN_CORE_ATOMICINTRINSICS_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/config.hpp>

#ifdef XEN_OS_UNIX
	#include <xen/core/atomic_intrinsics.unix.hpp>
#elif defined XEN_OS_WINDOWS
	#include <xen/core/atomic_intrinsics.win.hpp>
#else
	#error Atomic intrinsics not supported on this platform
#endif


#endif
