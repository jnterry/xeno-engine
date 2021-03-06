////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_HXX
#define XEN_KERNEL_KERNEL_HXX

#include <xen/kernel/Kernel.hpp>
#include <xen/core/time.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>

#include <xen/config.hpp>
#ifdef XEN_OS_WINDOWS
	#include "Kernel.win.hxx"
#elif defined XEN_OS_UNIX
	#include "Kernel.unix.hxx"
#else
  #error "Kernel is not implemented on this platform"
#endif

namespace xen {
	struct Module;
}

namespace xke {
	struct DynamicLibrary;
	struct ThreadData;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents and Module currently resident in memory
	/// \note Extends
	/////////////////////////////////////////////////////////////////////
	struct LoadedModule : public ModuleSource {
		/// \brief The Module instances exported by the library
		xen::Module*    module;

		/// \brief The data returned by module->initialise
		void*           data;

		/// \brief The api returned by module->onLoad
		void*           api;

		/// \brief Parameters to the module passed to init and load functions
		const void*     params;

		/// \brief Next pointer in singly linked list of currently loaded modules
		LoadedModule*   next;
	};

	xen::Module* platformLoadModule           (const char* name, ModuleSource* msrc);
	xen::Module* platformReloadModuleIfChanged(ModuleSource* msrc);
	bool         platformUnloadModule         (ModuleSource* msrc);

	struct Kernel {
		enum State {
			/// \brief initKernel has not yet been called
			UNINITIALIZED,

			/// \brief initKernel has been called, but startKernel has not
			INITIALIZED,

			/// \brief startKernel has been called, kernel has entered main loop
		  RUNNING,

			/// \brief Kernel main loop has terminated
			STOPPED,

			/// \brief Cleanup following main loop termination has been completed
			SHUTDOWN,
		};

		State state;

		xen::KernelSettings settings;

		xen::Allocator* root_allocator;

		/// \brief Arena for variable size kernel internal data
		/// which lasts the lifetime of the kernel
		xen::ArenaLinear system_arena;

		xen::ArenaPool<xke::LoadedModule> modules;
		xke::LoadedModule* module_head; // head of singly linked list of modules

		volatile bool stop_requested;
	};

	void platformRegisterSignalHandlers();

	/// \brief Global kernel instance
	extern Kernel kernel;
}

#endif
