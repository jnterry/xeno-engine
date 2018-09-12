////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Includes platform specific implementation for sren::RenderTargetImpl
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_CPP
#define XEN_SREN_RENDERTARGETIMPL_CPP

#include <xen/config.hpp>

#ifdef XEN_OS_UNIX
	#include "RenderTargetImpl.unix.cpp"
#elif defined XEN_OS_WINDOWS
	#include "RenderTargetImpl.win.cpp"
#else
	#error "Software renderer not supported on this platform!"
#endif

#endif
