////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of xen::Context and related types
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_CONTEXT_HPP
#define XEN_KERNEL_CONTEXT_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>

namespace xen {

	struct Context;

	/// \brief Type representing a function which
	typedef bool (*TickFunction)(const Context& cntx);

	/////////////////////////////////////////////////////////////////////
	/// \brief Pack of settings used to initialise the kernel
	/////////////////////////////////////////////////////////////////////
	struct KernelSettings {
		/// \brief Function which represents all computation that should occur
		/// in one frame of the application
		TickFunction loop;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Context of the engine containing all state passed to a call
	/// of MainLoopFunction
	/////////////////////////////////////////////////////////////////////
	struct Context {
		/// \brief Time since the kernel started
		xen::Duration time;

		/// \brief Delta time since the last call to loop
		xen::Duration dt;

		/// \brief Integer which is incremented by one just after
		/// each call to loop
		u64 tick;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Initialises Xenogin's kernel, after which it will begin calling the
	/// tick function. This function will not return until the tick function
	/// returns false
	/////////////////////////////////////////////////////////////////////
	void startKernel(const KernelSettings& settings);
}

#endif
