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
#include <xen/core/memory/Allocator.hpp>

namespace xen {

	struct Kernel {
		xen::Allocator* root_allocator;
	};

	Kernel* createKernel(const KernelSettings& settings){
		xen::AllocatorMalloc* allocator = new AllocatorMalloc();

		Kernel* kernel = (Kernel*)allocator->allocate(sizeof(Kernel));

		kernel->root_allocator = allocator;

		return kernel;
	}

	void startKernel(Kernel* kernel, TickFunction tick_function){
		TickContext cntx = {0};
		cntx.kernel = kernel;

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
