////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Declaration of module type and related functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_MODULE_HPP
#define XEN_KERNEL_MODULE_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>
#include <xen/core/String.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Struct containing all state passed to the tick callback
	/////////////////////////////////////////////////////////////////////
	struct TickContext {
		/// \brief Time since the kernel started
		xen::Duration time;

		/// \brief Delta time since the last call to the tick function
		xen::Duration dt;

		/// \brief Integer which is incremented after each call to the tick function
		u64 tick;

		/// \brief Data returned from module's initialize function
		void* data;

		const void* params;
	};

  /////////////////////////////////////////////////////////////////////
	/// \brief Represents the set of callbacks/functions that a Module should
	/// expose to the Kernel
	/////////////////////////////////////////////////////////////////////
	struct Module {
		/// \brief Callback where the module should initialize itself
		/// \param params The params passed to loadModule
		typedef void* (*FunctionInitialize)(const void* params);

		/// \brief Callback where the module should free all resources
		/// \param data The data returned by module initialize function
		/// \param params The params passed to loadModule
		typedef void  (*FunctionShutdown  )(void* data, const void* params);

		/// \brief Callback in which the module should load its code, this is any
		/// setup to be performed when the dll is (re)loaded, the returned data
		/// is considered to be the module's API (and hence should be a type with
		/// function pointers to the loaded code)
		/// \param data The data returned by the modules initialize function
		/// \param params The params passed to loadModule
		typedef void* (*FunctionLoad      )(void* data, const void* params);


		/// \brief Callback in which the module should clean up any resources
		/// allocated by load
		typedef void  (*FunctionUnload      )(void* data, const void* params);

		/// \brief Callback in which the module should perform any per tick
		/// processing
		typedef void  (*FunctionTick)      (const TickContext& tick);

		/////////////////////////////////////////////////////////////////////
		/// \brief xen::hash of the name of type of this module, this is used to
		/// support fetching module by type name, for example "graphics", "sound",
		/// etc. Note that type names may be fulfilled by multiple modules, for
		/// example, "graphics" may have an OpenGL implementation, DirectX, etc
		/////////////////////////////////////////////////////////////////////
		const StringHash type_hash;

		/////////////////////////////////////////////////////////////////////
		/// \brief Function which should be called exactly once in order to perform
		/// any initial setup of this module, this will be called by the Kernel the
		/// first time the module is loaded
		/////////////////////////////////////////////////////////////////////
		FunctionInitialize initialize;

		/////////////////////////////////////////////////////////////////////
		/// \brief Function which should be called exactly once in order to free
		/// any resources allocated/used etc by this module just before the
		/// application terminates
		/////////////////////////////////////////////////////////////////////
	  FunctionShutdown shutdown;

		/////////////////////////////////////////////////////////////////////
		/// \brief Function called by the kernel any time this module is (re)loaded.
		///
		/// The data pointer passed to this function is that which was returned by
		/// the initialize function, it is expected that the module should store
		/// any global state it required in this memory block (or at least
		/// accessible from this block via a pointer), this ensures if the module is
		/// dynamically reloaded during the applications run time due to a code
		/// change state will be persisted across reloads
		/////////////////////////////////////////////////////////////////////
	  FunctionLoad load;

		/////////////////////////////////////////////////////////////////////
		/// \brief Function called by the kernel just before the modules' dynamic
		/// link library is unloaded
		/////////////////////////////////////////////////////////////////////
		FunctionUnload unload;

		/////////////////////////////////////////////////////////////////////
		/// \brief Callback which is called once per kernel tick during which
		/// the module should perform any computation required (or more likely
		/// submit work to the kernel job queues)
		/////////////////////////////////////////////////////////////////////
	  FunctionTick tick;
	};
}

// Unfortuately modules cannot be declared in the same way across platforms.
// On windows we use GetProcAddress to fetch functions from a loaded dll,
// and on unix we us dlsym to get a symbol from the loaded so. Note that
// function pointers cannot be casted to data pointers and vice versa, hence
// these systems are incompatible. Hence under windows we must export a
// function which returns the xen::Module where as under unix we export the
// xen::Module instance itself. To ensure code is not duplicated in each
// module we use the macro XenDeclareModule, which is defined for each platform
// in the files listed below.
//
// (this also gives us the opertunity to fix up various other issues on
// windows... see the comment in the windows file)
#include <xen/config.hpp>
#ifdef XEN_OS_WINDOWS
#include "Module.win.hpp"
#elif defined XEN_OS_UNIX
#include "Module.unix.hpp"
#else
  #error "Kernel is not implemented on this platform"
#endif




#endif
