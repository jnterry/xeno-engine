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

void xen::log(u08 level, const char* file, u32 line, const char* message, ...){
	LogMessage msg;
	msg.time     = xen::getLocalTime();

	char buffer[8196];

	va_list args;
	va_start(args, message);
	vsnprintf(buffer, XenArrayLength(buffer), message, args);
	va_end(args);

	msg.level        = level;
	msg.thread       = xen::getThreadIndex();
	msg.file         = file;
	msg.line_number  = line;
	msg.message      = buffer;


	xke::printLogMsgToStdio(msg);
}

#include <xen/config.hpp>
#ifdef XEN_OS_WINDOWS
	#include "log.win.cpp"
#elif defined XEN_OS_UNIX
	#include "log.unix.cpp"
#else
	#include "log.dummy.cpp"
#endif

#endif
