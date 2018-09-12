////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for loading and then using dynamic
/// link libraries
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNE_DYNAMICLIBRARY_HPP
#define XEN_KERNE_DYNAMICLIBRARY_HPP

namespace xen {

	struct Allocator;

	/////////////////////////////////////////////////////////////////////
	/// \brief OS specific opaque type representing a dynamically loaded
	/// library
	/////////////////////////////////////////////////////////////////////
	struct DynamicLibrary;

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads a dynamic link library at some specified path, note
	/// that path SHOULD NOT have an extension, instead the OS's default
	/// extension is used (.dll for windows, .so for linux)
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

#endif
