////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Platform unix implementation of DynamicLibrary
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_DYNAMICLIBRARY_UNIX_CPP
#define XEN_KERNEL_DYNAMICLIBRARY_UNIX_CPP

#include <xen/kernel/DynamicLibrary.hpp>

#include <dlfcn.h>

#include <cstdio>

namespace xen {

	// under unix the dynamic library handle is just a void*, this type
	// isn't needed, we will just cast the void* to DynamicLibrary
	struct DynamicLibrary {};

	DynamicLibrary* loadDynamicLibrary(xen::Allocator& alloc, const char* path){
		char buffer[4096];
		unsigned i;
		for(i = 0; path[i] != '\0' && i < 4093; ++i){
			buffer[i] = path[i];
		}
		buffer[i+0] = '.';
		buffer[i+1] = 's';
		buffer[i+2] = 'o';
		buffer[i+3] = '\0';

		void* result = dlopen(buffer, RTLD_NOW);

		if(result == nullptr){
			// :TODO: log
			printf("Error loading dynamic library at '%s', error: %s\n", buffer, dlerror());
		}

		return (DynamicLibrary*)result;
	}

	void unloadDynamicLibrary(xen::Allocator& alloc, DynamicLibrary* lib){
		if(dlclose(lib) != 0){
			// :TODO: log
			printf("Failed to close dynamic library: %s\n", dlerror());
		}
	}

	void* getDynamicLibrarySymbol(DynamicLibrary* lib, const char* name){
	  void* result = dlsym(lib, name);
	  if(result == nullptr){
		  // :TODO: log
		  printf("Failed to load symbol from dynamic library: %s\n", dlerror());
	  }
	  return result;
	}

}

#endif
