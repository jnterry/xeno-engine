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

	/// \brief Opaque handle to a module loaded by the Kernel
	typedef void* ModuleHandle;

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads a kernel module and call's the module's init function
	/// \return Id of the loaded module which may be used later to fetch
	/// the module's exposed api
	/////////////////////////////////////////////////////////////////////
  ModuleHandle loadModule(Kernel& kernel, const char* lib_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the API exposed by some module. Note that this
	/// should not be cached between ticks, as if the kernel setting
	/// hot_reload_modules is enabled then it can change upon reload
	/////////////////////////////////////////////////////////////////////
	void* getModuleApi(Kernel& kernel, ModuleHandle module);

	/////////////////////////////////////////////////////////////////////
	/// \brief Allows a module to allocate memory through the kernel
	/////////////////////////////////////////////////////////////////////
	void* allocate(Kernel& kernel, u32 size, u32 align = alignof(int));

	/////////////////////////////////////////////////////////////////////
	/// \brief Allows a module to deallocate memory previously allocated
	/// through the kernel
	/////////////////////////////////////////////////////////////////////
	void deallocate(Kernel& kernel, void* data);

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
