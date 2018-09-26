////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for loading and then using dynamic
/// link libraries
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_DYNAMICLIBRARY_HPP
#define XEN_KERNEL_DYNAMICLIBRARY_HPP

#include <xen/config.hpp>

namespace xen {
	struct Allocator;
	struct ArenaLinear;
}

namespace xke {
	/////////////////////////////////////////////////////////////////////
	/// \brief OS specific opaque type representing a dynamically loaded
	/// library
	/////////////////////////////////////////////////////////////////////
	struct DynamicLibrary;

	/////////////////////////////////////////////////////////////////////
	/// \brief Resolves a dynamic library name to a path string
	/// Tries adding common extensions to name (depending on platform, IE:
	/// so for unix, dll for windows), searching in directory with executable,
	/// searching in cwd, etc
	///
	/// \return Path to the dynamic library, or nullptr if the name could not
	/// be resolved. Note that this pointer is into the thread scratch space,
	/// and hence is only valid until the end of the current tick
	/////////////////////////////////////////////////////////////////////
  char* resolveDynamicLibrary(const char* name);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads a dynamic link library at some specified path
	/////////////////////////////////////////////////////////////////////
	DynamicLibrary* loadDynamicLibrary(xen::Allocator& alloc, const char* path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves address of a function in some dynamic link library
	/////////////////////////////////////////////////////////////////////
	void* getDynamicLibrarySymbol(DynamicLibrary* lib, const char* name);

	/////////////////////////////////////////////////////////////////////
	/// \brief Unloaded a previously loaded dynamic link library, freeing
	/// storage used. Note this only frees the memory for the OS specific data
	/// about the dynamic library - it DOES NOT free memory allocated by
	/// functions called that belong to the library - any appropriate cleanup
	/// methods should be called first!
	/////////////////////////////////////////////////////////////////////
	void unloadDynamicLibrary(xen::Allocator& alloc, DynamicLibrary* lib);
}

#ifdef XEN_OS_WINDOWS
	#include "DynamicLibrary.win.hxx"
#elif defined XEN_OS_UNIX
	#include "DynamicLibrary.unix.hxx"
#else
	#error "Dynamic libraries not implemented on this platform"
#endif

#endif
