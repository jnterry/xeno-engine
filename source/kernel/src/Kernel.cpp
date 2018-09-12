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
#include "DynamicLibrary.hxx"
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/ArenaPool.hpp>

#include <utility>
#include <new>

namespace xen {

	struct LoadedModule {
		ModuleSource    source;
		DynamicLibrary* library;
		Module*         module;
		LoadedModule*   next; // next pointer in singly linked list
	};

	struct Kernel {
		xen::Allocator& root_allocator;

		xen::ArenaPool<LoadedModule> modules;
		LoadedModule* module_head; // head of singly linked list of modules

		Kernel(xen::Allocator* root_alloc)
			: root_allocator(*root_alloc),
			  modules(xen::createArenaPool<LoadedModule>(root_alloc, 128)) {

		}
	};

	Kernel& createKernel(const KernelSettings& settings){
		xen::AllocatorMalloc* allocator = new AllocatorMalloc();

		void* mem = allocator->allocate(sizeof(Kernel));
		Kernel* kernel = new (mem) (Kernel)(allocator);

		return *kernel;
	}

	Module* loadModule(Kernel& kernel, const ModuleSource& source){
		LoadedModule* module = xen::reserveType<LoadedModule>(kernel.modules);

		if(module == nullptr){
			// :TODO: log
			printf("Failed to load module as max number of loaded modules has been reached!\n");
			goto cleanup;
		}

		module->next       = kernel.module_head;
		kernel.module_head = module;

		module->library = xen::loadDynamicLibrary(kernel.root_allocator, source.lib_path);

		if(module->library == nullptr){
			// :TODO: log
			printf("Failed to load module shared library\n");
			goto cleanup;
		}

		module->module = (Module*)xen::getDynamicLibrarySymbol(module->library, "exported_xen_module");

		if(module->module == nullptr){
			// :TODO: log
			printf("Cannot load the shared library '%s' as a kernel module "
			       "- expected a symbol 'exported_xen_module' to be present\n",
			       source.lib_path
			      );
			goto cleanup;
		}

		if(!module->module->init(kernel)){
			// :TODO: log
			printf("Failed to initizalise module '%s'\n",
			       source.lib_path
			      );
			goto cleanup;
		}

		return module->module;

	cleanup:
		if(module == nullptr){ return nullptr; }

		if(module->library != nullptr){
			xen::unloadDynamicLibrary(kernel.root_allocator, module->library);
		}

		xen::freeType<LoadedModule>(kernel.modules, module);

		return nullptr;
	}

	void startKernel(Kernel& kernel, TickFunction tick_function){
		TickContext cntx = {kernel, 0};

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

		LoadedModule* module = kernel.module_head;
		while(module != nullptr){
			module->module->shutdown(kernel);
			module = module->next;
		}

		// free resources, check for memory leaks, etc
		printf("Kernel terminating\n");
	}
}


#endif
