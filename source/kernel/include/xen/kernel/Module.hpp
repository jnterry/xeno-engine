////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Declaration of module type and related functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_MODULE_HPP
#define XEN_KERNEL_MODULE_HPP

namespace xen {

	struct Kernel;

  /////////////////////////////////////////////////////////////////////
	/// \brief Represents the set of callbacks/functions that a Module should
	/// expose to the Kernel
	/////////////////////////////////////////////////////////////////////
	struct Module {
		typedef void* (*FunctionInitialize)(Kernel& kernel);
		typedef void  (*FunctionShutdown  )(Kernel& kernel);
		typedef void* (*FunctionLoad      )(Kernel& kernel, void* data);

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
	};
}

#endif
