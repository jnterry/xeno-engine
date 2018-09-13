////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of xen::Context and related types
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_HPP
#define XEN_KERNEL_KERNEL_HPP

#include <xen/kernel/Module.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>

namespace xen {

	struct Allocator;

	/////////////////////////////////////////////////////////////////////
	/// \brief Pack of settings used to initialise the kernel
	/////////////////////////////////////////////////////////////////////
	struct KernelSettings {
		/// \brief If true then before each tick the library file for any loaded
		/// modules will be checked for modifications, and be reloaded if necessary
		bool hot_reload_modules;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Opaque type representing all kernel state
	/////////////////////////////////////////////////////////////////////
	struct Kernel;

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new Kernel
	/////////////////////////////////////////////////////////////////////
	Kernel& createKernel(const KernelSettings& settings);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads a kernel module and call's the module's init function
	/////////////////////////////////////////////////////////////////////
	Module* loadModule(Kernel& kernel, const char* lib_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Struct containing all state passed to the TickFunction
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

	/// \brief Type representing a function which
	typedef bool (*TickFunction)(const TickContext& cntx);

	/////////////////////////////////////////////////////////////////////
	/// \brief Initialises Xenogin's kernel, after which it will begin calling the
	/// tick function. This function will not return until the tick function
	/// returns false
	/////////////////////////////////////////////////////////////////////
	void startKernel(Kernel& kernel, TickFunction tick_function);
}

#endif
