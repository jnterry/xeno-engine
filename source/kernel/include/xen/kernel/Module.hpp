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

		/// \brief Callback where the module should free resources
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
		/// \brief Callback which is called once per kernel tick during which
		/// the module should perform any computation required (or more likely
		/// submit work to the kernel job queues)
		/////////////////////////////////////////////////////////////////////
	  FunctionTick tick;
	};
}

#endif
