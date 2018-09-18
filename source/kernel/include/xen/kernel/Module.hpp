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

namespace xen {

	struct Kernel;

	/////////////////////////////////////////////////////////////////////
	/// \brief Struct containing all state passed to the tick callback
	/////////////////////////////////////////////////////////////////////
	struct TickContext {
		/// \brief The kernel causing this tick to occur
		Kernel& kernel;

		/// \brief Time since the kernel started
		xen::Duration time;

		/// \brief Delta time since the last call to the tick function
		xen::Duration dt;

		/// \brief Integer which is incremented after each call to the tick function
		u64 tick;
	};

  /////////////////////////////////////////////////////////////////////
	/// \brief Represents the set of callbacks/functions that a Module should
	/// expose to the Kernel
	/////////////////////////////////////////////////////////////////////
	struct Module {
		typedef void* (*FunctionInitialize)(Kernel& kernel, const void* params);
		typedef void  (*FunctionShutdown  )(Kernel& kernel);
		typedef void* (*FunctionLoad      )(Kernel& kernel, void* data, const void* params);
		typedef void  (*FunctionTick)      (Kernel& kernel, const TickContext& tick);

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
