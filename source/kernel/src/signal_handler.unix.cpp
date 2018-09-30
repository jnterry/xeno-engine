////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the kernel signal handler for unix
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_SIGNALHANDLER_UNIX_CPP
#define XEN_KERNEL_SIGNALHANDLER_UNIX_CPP

// sigsegv handler includes
#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

namespace {
	void sigsegvHandler(int sig) {
		void* array[256];
		size_t size;

		// get void*'s for all entries on the stack
		size = backtrace(array, 256);

		// print out all the frames to stderr
		fprintf(stderr, "Error: signal SIGSEGV\n");
		backtrace_symbols_fd(array, size, STDERR_FILENO);
		exit(1);
	}
}

namespace xke {

}

#endif
