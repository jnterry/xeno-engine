////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Platform agnostic implementation of DynamicLibrary
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_DYNAMICLIBRARY_CPP
#define XEN_KERNEL_DYNAMICLIBRARY_CPP

#include <xen/config.hpp>


#ifdef XEN_OS_WINDOWS
	#error "Dynamic libraries not implemented on windows"
#elif defined XEN_OS_UNIX
	#include "DynamicLibrary.unix.cpp"
#else
	#error "Dynamic libraries not implemented on this platform"
#endif

#endif
