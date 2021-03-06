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
#include <xen/core/String.hpp>

namespace xen {

	struct Allocator;

	/////////////////////////////////////////////////////////////////////
	/// \brief Pack of settings used to initialise the kernel
	/////////////////////////////////////////////////////////////////////
	struct KernelSettings {
		/// \brief If true then before each tick the library file for any loaded
		/// modules will be checked for modifications, and be reloaded if necessary
		bool hot_reload_modules;

		/// \brief If true then the kernel will print the average tick rate once
		/// a second to stdout
		bool print_tick_rate;

		/// \brief Number of threads to use, if set to 0 then will use as many
		/// threads as there are cores in this machine
		u64 thread_count;

		/// \brief Size in bytes of the arena returned by getThreadScratchSpace
		u64 thread_scratch_size;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs initialisation of the global kernel instance, this
	/// must be called before any other kernel functions may be called
	/////////////////////////////////////////////////////////////////////
  bool initKernel(const KernelSettings& settings);

	/////////////////////////////////////////////////////////////////////
	/// \brief Starts running the kernel, which basically amounts to repeatedly
	/// calling tick for all loaded modules. This function will not return
	/// until requestKernelShutdown is called (hence requestKernelShutdown must be
	/// called within the tick() callback of some loaded module)
	/////////////////////////////////////////////////////////////////////
	void startKernel();

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads a kernel module and call's the module's init function
	///
	/// \param params Data passed to the module's init and load functions in order
	/// to configure the modules behaviour. Note that the kernel expects this
	/// pointer  to be valid for the entire lifetime of the kernel, since the
	/// params may need to be reused at any point if the hot_reload_modules
	/// is true
	///
	/// \return Id of the loaded module which may be used later to fetch
	/// the module's exposed api - will return 0 if failed to load the module
	/////////////////////////////////////////////////////////////////////
	StringHash loadModule(const char* lib_path, const void* params = nullptr);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the API exposed by some module. Note that this
	/// should not be cached between ticks, as if the kernel setting
	/// hot_reload_modules is enabled then it can change upon reload
	/////////////////////////////////////////////////////////////////////
	void* getModuleApi(u64 module_type_hash);

	inline void* getModuleApi(const char* type_name){
		return getModuleApi(xen::hash(type_name));
	}

	template<typename T>
	T* getModuleApi(){
		return (T*)getModuleApi(xen::hash(T::NAME));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Allows a module to allocate memory through the kernel
	/// \note This may be called before startKernel, but not before initKernel
	/////////////////////////////////////////////////////////////////////
	void* kernelAlloc(u32 size, u32 align = alignof(int));

	/////////////////////////////////////////////////////////////////////
	/// \brief Allows a module to deallocate memory previously allocated
	/// through the kernel with kernelAlloc
	/// \note This may be called before startKernel, but not before initKernel
	/////////////////////////////////////////////////////////////////////
	void kernelFree(void* data);

	/////////////////////////////////////////////////////////////////////
	/// \brief Requests that the kernel shutdown once the current tick is
	/// complete
	/////////////////////////////////////////////////////////////////////
	void requestKernelShutdown();
}

#endif
