////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of kernel logging functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_LOG_CPP
#define XEN_KERNEL_LOG_CPP

#include "log.hxx"
#include <xen/kernel/threads.hpp>

#include <cstdio>
#include <cstdarg>

void xen::logv(u08 level, const char* file, u32 line, const char* message, va_list args){
	LogMessage msg;
	msg.time     = xen::getLocalTime();

	char buffer[8196];

	vsnprintf(buffer, XenArrayLength(buffer), message, args);

	msg.level        = level;
	msg.thread       = xen::getThreadIndex();
	msg.file         = file;
	msg.line_number  = line;
	msg.message      = buffer;

	xke::printLogMsgToStdio(msg);
}

#include <xen/config.hpp>
#ifdef XEN_OS_WINDOWS
	#include "log.dummy.cpp"
#elif defined XEN_OS_UNIX
	#include "log.unix.cpp"
#else
	#include "log.dummy.cpp"
#endif

#endif
