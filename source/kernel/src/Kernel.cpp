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
#include <xen/core/File.hpp>\

#include <utility>
#include <new>

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

		/// \brief Next pointer in singly linked list of currently loaded modules
		LoadedModule*   next;
	};

	struct Kernel {
		KernelSettings settings;

		xen::Allocator& root_allocator;

		xen::ArenaPool<LoadedModule> modules;
		LoadedModule* module_head; // head of singly linked list of modules

		Kernel(xen::Allocator* root_alloc)
			: root_allocator(*root_alloc),
			  modules(xen::createArenaPool<LoadedModule>(root_alloc, 128)) {

		}
	};

}

namespace {
	void reloadModifiedKernelModules(xen::Kernel& kernel){
		xen::LoadedModule* cur;
	  for(cur = kernel.module_head; cur != nullptr; cur = cur->next){
		  xen::DateTime mod_time = xen::getPathModificationTime(cur->lib_path);

		  printf("Mod time: %lu, load mod time: %lu\n",
		         mod_time._data,
		         cur->lib_modification_time
		        );

		  if(mod_time > cur->lib_modification_time){
			  printf("Need to reload: %s\n", cur->lib_path);
		  }
		}
	}
}


namespace xen {
	Kernel& createKernel(const KernelSettings& settings){
		xen::AllocatorMalloc* allocator = new AllocatorMalloc();

		void* mem = allocator->allocate(sizeof(Kernel));
		Kernel* kernel = new (mem) (Kernel)(allocator);

		xen::copyBytes(&settings, &kernel->settings, sizeof(KernelSettings));

		return *kernel;
	}

  void* loadModule(Kernel& kernel, const char* name){
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

		lmod->lib_path   = lib_path;
		lmod->next       = kernel.module_head;
		kernel.module_head = lmod;

	  lmod->library = xen::loadDynamicLibrary(kernel.root_allocator, lib_path);

		if(lmod->library == nullptr){
			// :TODO: log
			printf("Failed to load module shared library\n");
			goto cleanup;
		}

	  lmod->module = (Module*)xen::getDynamicLibrarySymbol(lmod->library, "exported_xen_module");

		if(lmod->module == nullptr){
			// :TODO: log
			printf("Cannot load the shared library '%s' as a kernel module "
			       "- expected a symbol 'exported_xen_module' to be present\n",
			       lib_path
			      );
			goto cleanup;
		}

		printf("Initializing module: %s\n", name);
		if(lmod->module->initialize == nullptr){
			lmod->data = nullptr;
		} else {
			lmod->data = lmod->module->initialize(kernel);
			if(lmod->data == nullptr){
				printf("Failed to initizalise module '%s'\n", name);
				goto cleanup;
			}
		}

		// :TODO: If a module does not export an API we don't technically need a
		// load function, however we need to return something from this function
		// other than nullptr so that caller to loadModule knows that it was
		printf("Loading module's API: %s\n", name);
		if(lmod->module->load == nullptr){
			lmod->api = (void*)true;
		} else {
			lmod->api = lmod->module->load(kernel, lmod->data);
			if(lmod->api == nullptr){
				printf("Module's load function returned nullptr, module: '%s'\n", name);
				goto cleanup;
			}
		}

		printf("Finished initializing and loading module: %s\n", name);
		return lmod->api;

	cleanup:
		printf("Cleaning up from failed module load: %s\n", name);
		if(lmod == nullptr){ return nullptr; }

		if(lib_path != nullptr){
			kernel.root_allocator.deallocate(lib_path);
		}

		if(lmod->library != nullptr){
			xen::unloadDynamicLibrary(kernel.root_allocator, lmod->library);
		}

		xen::freeType<LoadedModule>(kernel.modules, lmod);

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

			if(kernel.settings.hot_reload_modules){
				reloadModifiedKernelModules(kernel);
			}

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

	void* getModuleApi(Kernel& kernel, ModuleHandle module){
		return ((LoadedModule*)module)->api;
	}

	void* allocate(Kernel& kernel, u32 size, u32 align){
		printf("Allocating via kernel\n");
		// :TODO: we ideally want an allocator per module so we can debug memory
		// leaks etc
		return kernel.root_allocator.allocate(size, align);
	}

	void deallocate(Kernel& kernel, void* data){
		printf("Deallocating via kernel\n");
		return kernel.root_allocator.deallocate(data);
	}
}


#endif
