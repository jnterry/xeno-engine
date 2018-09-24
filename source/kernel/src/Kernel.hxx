////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_HXX
#define XEN_KERNEL_KERNEL_HXX

namespace xen {
	struct Module;
}

namespace xke {
	struct DynamicLibrary;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents and Module currently resident in memory
	/////////////////////////////////////////////////////////////////////
	struct LoadedModule {
		/// \brief The path to the shared library file containing code for module
		const char*     lib_path;

		/// \brief The modification time of the lib the last time it was loaded
		xen::DateTime   lib_modification_time;

		/// \brief DynamicLibrary instance representing loaded code for the module
		xke::DynamicLibrary* library;

		/// \brief The Module instances exported by the library
		xen::Module*         module;

		/// \brief The data returned by module->initialise
		void*           data;

		/// \brief The api returned by module->onLoad
		void*           api;

		/// \brief Parameters to the module passed to init and load functions
		const void*     params;

		/// \brief Next pointer in singly linked list of currently loaded modules
		LoadedModule*   next;
	};
};

namespace xen {


	struct Kernel {
		KernelSettings settings;

		xen::Allocator* root_allocator;

		xen::ArenaPool<xke::LoadedModule> modules;
		xke::LoadedModule* module_head; // head of singly linked list of modules

		bool stop_requested;

		// Should this be per thread?
		ArenaLinear tick_scratch_space;
	};
}

#endif
