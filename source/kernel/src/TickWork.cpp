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

#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include "TickWork.dummy.cpp"
#elif defined XEN_OS_UNIX
	#include "TickWork.dummy.cpp"
#else
	#include "TickWork.dummy.cpp"
#endif

#endif
