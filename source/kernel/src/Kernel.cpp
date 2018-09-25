////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of types defined in Kernel.hpp
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_CONTEXT_CPP
#define XEN_KERNEL_CONTEXT_CPP

#include <xen/kernel/Kernel.hpp>
#include "DynamicLibrary.hxx"
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>

#include <utility>
#include <new>

// sigsegv handler includes
#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#include "Kernel.hxx"

namespace {

	bool doModuleLoad(xen::Kernel& kernel, xke::LoadedModule* lmod){
		printf("Loading shared library: %s\n", lmod->lib_path);
		lmod->library = xke::loadDynamicLibrary(*kernel.root_allocator, lmod->lib_path);

		if(lmod->library == nullptr){
			// :TODO: log
			printf("Failed to load module shared library\n");
			return false;
		}


		lmod->module = (xen::Module*)xke::getDynamicLibrarySymbol(lmod->library, "exported_xen_module");

		if(lmod->module == nullptr){
			// :TODO: log
			printf("Cannot load the shared library '%s' as a kernel module "
			       "- expected a symbol 'exported_xen_module' to be present\n",
			       lmod->lib_path
			      );
			return false;
		}

		if(lmod->data == nullptr){
			printf("Initializing module: %s\n", lmod->lib_path);
			if(lmod->module->initialize == nullptr){
				lmod->data = nullptr;
			} else {
				lmod->data = lmod->module->initialize(kernel, lmod->params);
				if(lmod->data == nullptr){
					printf("Failed to initizalise module '%s'\n", lmod->lib_path);
					return false;
				}
			}
		}

		printf("Loading module's API: %s\n", lmod->lib_path);
		if(lmod->module->load == nullptr){
			lmod->api = (void*)true;
		} else {
			lmod->api = lmod->module->load(kernel, lmod->data, lmod->params);
			if(lmod->api == nullptr){
				printf("Module's load function returned nullptr, module: '%s'\n", lmod->lib_path);
				return false;
			}
		}

		return true;
	}

	void reloadModifiedKernelModules(xen::Kernel& kernel){
		for(xke::LoadedModule* lmod = kernel.module_head;
		    lmod != nullptr;
		    lmod = lmod->next
		   ){

		  xen::DateTime mod_time = xen::getPathModificationTime(lmod->lib_path);

		  //printf("Mod time: %lu, load mod time: %lu\n",
		  //       mod_time._data,
		  //       lmod->lib_modification_time
		  //      );

		  if(mod_time > lmod->lib_modification_time){
			  xen::DateTime now = xen::getLocalTime();

			  if(now - mod_time < xen::seconds(1.5f)){
				  // :TODO: this is a nasty hack - issue is that the linker will
				  // truncate the file, then begin writing to it. Initial truncation
				  // changes modification time, so we may start trying to load the
				  // module before the linker has finished writing it. This says that
				  // we should only load the module if it changed AND that was at least
				  // 1 second ago. If linker takes longer than 1 second this will blow
				  // up
				  //printf("Need to reload %s but too recently modified\n", lmod->lib_path);
				  continue;
			  }

			  printf("Reloading: %s\n", lmod->lib_path);
			  // :TODO: would be better to attempt to load new version
			  // before we let go of the old version
			  // But windows/unix etc doesn't let us load the same dynamic
			  // library more than once (we could make a copy of the library
			  // file and load the copy, hence tricking the OS loader to let us
			  // load a dll multiple times)
			  xke::unloadDynamicLibrary(*kernel.root_allocator, lmod->library);

			  doModuleLoad(kernel, lmod);
			  lmod->lib_modification_time = mod_time;
		  }
		}
	}

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

xen::Kernel& xen::createKernel(const xen::KernelSettings& settings){
	signal(SIGSEGV, sigsegvHandler);

	xen::AllocatorMalloc* allocator = new xen::AllocatorMalloc();

	constexpr u32 SYSTEM_ARENA_SIZE = xen::kilobytes(16);

	xen::Kernel* kernel = (xen::Kernel*)allocator->allocate(SYSTEM_ARENA_SIZE);

	kernel->root_allocator = allocator;

	kernel->system_arena.start     = xen::ptrGetAdvanced(kernel, sizeof(Kernel));
	kernel->system_arena.end       = xen::ptrGetAdvanced(kernel, SYSTEM_ARENA_SIZE);
	kernel->system_arena.next_byte = kernel->system_arena.start;

	xen::copyBytes(&settings, &kernel->settings, sizeof(xen::KernelSettings));

	kernel->modules            = xen::createArenaPool<xke::LoadedModule>(kernel->system_arena, 128);
	kernel->tick_scratch_space = xen::createArenaLinear(*allocator, xen::megabytes(16));


	if(!xke::initThreadSubsystem(kernel)){
		printf("Error occured while initializing thread subsystem of kernel\n");
		XenBreak();
	}

	printf("Finished kernel init, used %lu of %lu system arena bytes\n",
	       xen::ptrDiff(kernel->system_arena.start, kernel->system_arena.next_byte),
	       xen::ptrDiff(kernel->system_arena.start, kernel->system_arena.end)
	      );

	return *kernel;
}

xen::StringHash xen::loadModule(xen::Kernel& kernel,
                                const char* name,
                                const void* params
                               ){
	xke::LoadedModule* lmod = xen::reserveType<xke::LoadedModule>(kernel.modules);
	char* lib_path = nullptr;

	if(lmod == nullptr){
		// :TODO: log
		printf("Failed to load module as max number of loaded modules has been reached!\n");
		goto cleanup;
	}

	lib_path = xke::resolveDynamicLibrary(*kernel.root_allocator, name);
	if(lib_path == nullptr){
		printf("Failed to find library file for module: %s\n", name);
		goto cleanup;
	}
	lmod->lib_modification_time = xen::getPathModificationTime(lib_path);

	lmod->params     = params;
	lmod->lib_path   = lib_path;
	lmod->next       = kernel.module_head;
	kernel.module_head = lmod;

	if(!doModuleLoad(kernel, lmod)){
		goto cleanup;
	}

	printf("Finished initializing and loading module: %s\n", name);
	return lmod->module->type_hash;

	cleanup: {
		printf("Cleaning up from failed module load: %s\n", name);
		if(lmod == nullptr){ return 0; }

		if(lib_path != nullptr){
			kernel.root_allocator->deallocate(lib_path);
		}

		if(lmod->library != nullptr){
			xke::unloadDynamicLibrary(*kernel.root_allocator, lmod->library);
		}

		xen::freeType<xke::LoadedModule>(kernel.modules, lmod);

		return 0;
	}
}

void xen::startKernel(xen::Kernel& kernel){
	xen::TickContext cntx = {kernel, 0};

	xen::Stopwatch timer;
	xen::Duration last_time = timer.getElapsedTime();

	xen::Duration last_tick_rate_print = last_time;
	u64           last_tick_count      = 0;

	printf("Kernel init finished, beginning main loop...\n");
	do {
		xen::resetArena(kernel.tick_scratch_space);
		xke::preTickThreadSubsystem(&kernel);

		cntx.time = timer.getElapsedTime();
		cntx.dt = cntx.time - last_time;

		if(kernel.settings.hot_reload_modules){
			//printf("Checking for module reloads...\n");
			reloadModifiedKernelModules(kernel);
			//printf("Done reloads\n");
		}

		if(kernel.settings.print_tick_rate &&
		   cntx.time - last_tick_rate_print > xen::seconds(0.5f)){
			printf("Tick Rate: %f\n",
			       (real)(cntx.tick - last_tick_count) /
			       xen::asSeconds<real>(cntx.time - last_tick_rate_print)
			       );
			last_tick_rate_print = cntx.time;
			last_tick_count      = cntx.tick;
		}

		for(xke::LoadedModule* lmod = kernel.module_head;
		    lmod != nullptr;
		    lmod = lmod->next
		    ){
			if(lmod->module->tick != nullptr){
				lmod->module->tick(kernel, cntx);
			}
		}

		// Ensure that all tick work is completed
		xen::waitForTickWork(kernel, 0);

		++cntx.tick;
		last_time = cntx.time;
	} while (!kernel.stop_requested);

	printf("Main loop requested termination, doing kernel cleanup\n");

	xke::LoadedModule* module = kernel.module_head;
	while(module != nullptr){
		module->module->shutdown(kernel);
		module = module->next;
	}

	// free resources, check for memory leaks, etc
	printf("Kernel terminating\n");
	xke::stopThreadSubsystem(&kernel);
}

void* xen::getModuleApi(xen::Kernel& kernel, xen::StringHash hash){
	for(xke::LoadedModule* cur = kernel.module_head;
	    cur != nullptr;
	    cur = cur->next
	    ){
		if(cur->module->type_hash == hash){
			return cur->api;
		}
	}
	return nullptr;
}

void* xen::allocate(xen::Kernel& kernel, u32 size, u32 align){
	// :TODO: we ideally want an allocator per module so we can debug memory
	// leaks etc
	return kernel.root_allocator->allocate(size, align);
}

void xen::deallocate(xen::Kernel& kernel, void* data){
	return kernel.root_allocator->deallocate(data);
}

void xen::requestKernelShutdown(xen::Kernel& kernel){
	kernel.stop_requested = true;
}

xen::ArenaLinear& xen::getTickScratchSpace(xen::Kernel& kernel){
	return kernel.tick_scratch_space;
}

#endif
