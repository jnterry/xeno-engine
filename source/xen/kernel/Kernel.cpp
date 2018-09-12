////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of types defined in Context.hpp
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_CONTEXT_CPP
#define XEN_KERNEL_CONTEXT_CPP

#include <xen/kernel/Kernel.hpp>

namespace xen {

	void startKernel(TickFunction tick_function){
		TickContext cntx = {0};

		bool tick_result;

		xen::Stopwatch timer;
		xen::Duration last_time = timer.getElapsedTime();

		printf("Kernel init finished, beginning main loop...\n");
		do {
			cntx.time = timer.getElapsedTime();
			cntx.dt = cntx.time - last_time;

		  tick_result = tick_function(cntx);

			++cntx.tick;
			last_time = cntx.time;
		} while (tick_result);

		printf("Main loop requested termination, doing kernel cleanup\n");
		// free resources, check for memory leaks, etc
		printf("Kernel terminating\n");
	}
}


#endif
