////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains platform specific implementation of kernel logging functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_LOG_DUMMY_CPP
#define XEN_KERNEL_LOG_DUMMY_CPP

#include "log.hxx"
#include <xen/core/String.hpp>
#include <xen/core/memory/utilities.hpp>

bool xke::initLogSubsystem(){
	return true;
}

void xke::printLogMsgToStdio(const xen::LogMessage& lm){
	if(lm.level < xen::LogLevel::DEBUG){
		return;
	}

	printf("%s\n", lm.message);
}

#endif
