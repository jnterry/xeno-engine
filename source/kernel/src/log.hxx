////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Implementation details for logger
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_LOG_HXX
#define XEN_KERNEL_LOG_HXX

#include <xen/kernel/log.hpp>

namespace xke {
	/// \brief Name of the module currently being processed by this thread
	extern thread_local const char* THREAD_CURRENT_MODULE;

	void printLogMsgToStdio(const xen::LogMessage&);

	bool initLogSubsystem();
}

#endif
