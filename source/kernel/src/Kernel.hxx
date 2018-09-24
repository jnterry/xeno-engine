////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_HXX
#define XEN_KERNEL_KERNEL_HXX

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

		// Should this be per thread?
		ArenaLinear tick_scratch_space;

		Kernel(xen::Allocator* root_alloc)
			: root_allocator(*root_alloc),
			  modules(xen::createArenaPool<LoadedModule>(root_alloc, 128)) {

		}
	};
}

#endif
