////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of types defined in Context.hpp
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_CONTEXT_CPP
#define XEN_KERNEL_CONTEXT_CPP

#include <xen/kernel/Context.hpp>

namespace xen {

	void startKernel(const KernelSettings& settings){
		Context cntx = {0};

		xen::Stopwatch timer;
		bool loop_return_code;
		xen::Duration last_time = timer.getElapsedTime();

		printf("Kernel init finished, beginning main loop...\n");
		do {
			cntx.time = timer.getElapsedTime();
			cntx.dt = cntx.time - last_time;

			loop_return_code = settings.loop(cntx);

			++cntx.tick;
			last_time = cntx.time;
		} while (loop_return_code);

		printf("Main loop requested termination, doing kernel cleanup\n");
		// free resources, check for memory leaks, etc
		printf("Kernel terminating\n");
	}
}


#endif
