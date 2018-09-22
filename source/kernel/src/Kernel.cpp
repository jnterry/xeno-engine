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

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents and Module currently resident in memory
	/////////////////////////////////////////////////////////////////////
	struct LoadedModule {
		/// \brief The path to the shared library file containing code for module
		const char*     lib_path;

		/// \brief The modification time of the lib the last time it was loaded
		xen::DateTime   lib_modification_time;

		/// \brief DynamicLibrary instance representing loaded code for the module
		DynamicLibrary* library;

		/// \brief The Module instances exported by the library
		Module*         module;

		/// \brief The data returned by module->initialise
		void*           data;

		/// \brief The api returned by module->onLoad
		void*           api;

		/// \brief Parameters to the module passed to init and load functions
		const void*     params;

		/// \brief Next pointer in singly linked list of currently loaded modules
		LoadedModule*   next;
	};

	struct Kernel {
		KernelSettings settings;

		xen::Allocator& root_allocator;

		xen::ArenaPool<LoadedModule> modules;
		LoadedModule* module_head; // head of singly linked list of modules

		bool stop_requested;

		Kernel(xen::Allocator* root_alloc)
			: root_allocator(*root_alloc),
			  modules(xen::createArenaPool<LoadedModule>(root_alloc, 128)) {

		}
	};

}

namespace {

	bool doModuleLoad(xen::Kernel& kernel, xen::LoadedModule* lmod){
		printf("Loading shared library: %s\n", lmod->lib_path);
		lmod->library = xen::loadDynamicLibrary(kernel.root_allocator, lmod->lib_path);

		if(lmod->library == nullptr){
			// :TODO: log
			printf("Failed to load module shared library\n");
			return false;
		}


		lmod->module = (xen::Module*)xen::getDynamicLibrarySymbol(lmod->library, "exported_xen_module");

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
		for(xen::LoadedModule* lmod = kernel.module_head;
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
			  xen::unloadDynamicLibrary(kernel.root_allocator, lmod->library);

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
		fprintf(stderr, "Error: signal SIGSEGV\n", sig);
		backtrace_symbols_fd(array, size, STDERR_FILENO);
		exit(1);
	}
}

namespace xen {
	Kernel& createKernel(const KernelSettings& settings){
		signal(SIGSEGV, sigsegvHandler);

		xen::AllocatorMalloc* allocator = new AllocatorMalloc();

		void* mem = allocator->allocate(sizeof(Kernel));
		Kernel* kernel = new (mem) (Kernel)(allocator);

		xen::copyBytes(&settings, &kernel->settings, sizeof(KernelSettings));

		return *kernel;
	}

  StringHash loadModule(Kernel& kernel, const char* name, const void* params){
		LoadedModule* lmod = xen::reserveType<LoadedModule>(kernel.modules);
		char* lib_path = nullptr;

		if(lmod == nullptr){
			// :TODO: log
			printf("Failed to load module as max number of loaded modules has been reached!\n");
			goto cleanup;
		}

	  lib_path = xen::resolveDynamicLibrary(kernel.root_allocator, name);
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

	cleanup:
		printf("Cleaning up from failed module load: %s\n", name);
		if(lmod == nullptr){ return 0; }

		if(lib_path != nullptr){
			kernel.root_allocator.deallocate(lib_path);
		}

		if(lmod->library != nullptr){
			xen::unloadDynamicLibrary(kernel.root_allocator, lmod->library);
		}

		xen::freeType<LoadedModule>(kernel.modules, lmod);

		return 0;
	}

	void startKernel(Kernel& kernel){
		TickContext cntx = {kernel, 0};

		bool tick_result;

		xen::Stopwatch timer;
		xen::Duration last_time = timer.getElapsedTime();

		printf("Kernel init finished, beginning main loop...\n");
		do {
			cntx.time = timer.getElapsedTime();
			cntx.dt = cntx.time - last_time;

			if(kernel.settings.hot_reload_modules){
				//printf("Checking for module reloads...\n");
				reloadModifiedKernelModules(kernel);
				//printf("Done reloads\n");
			}

			for(LoadedModule* lmod = kernel.module_head;
			    lmod != nullptr;
			    lmod = lmod->next
			    ){
				if(lmod->module->tick != nullptr){
					lmod->module->tick(kernel, cntx);
				}
			}

			++cntx.tick;
			last_time = cntx.time;
		} while (!kernel.stop_requested);

		printf("Main loop requested termination, doing kernel cleanup\n");

		LoadedModule* module = kernel.module_head;
		while(module != nullptr){
			module->module->shutdown(kernel);
			module = module->next;
		}

		// free resources, check for memory leaks, etc
		printf("Kernel terminating\n");
	}

	void* getModuleApi(Kernel& kernel, StringHash hash){
		for(LoadedModule* cur = kernel.module_head;
		    cur != nullptr;
		    cur = cur->next
		   ){
			if(cur->module->type_hash == hash){
				return cur->api;
			}
		}
		return nullptr;
	}

	void* allocate(Kernel& kernel, u32 size, u32 align){
		// :TODO: we ideally want an allocator per module so we can debug memory
		// leaks etc
		return kernel.root_allocator.allocate(size, align);
	}

	void deallocate(Kernel& kernel, void* data){
		return kernel.root_allocator.deallocate(data);
	}

	void requestKernelShutdown(Kernel& kernel){
		kernel.stop_requested = true;
	}
}

#endif
