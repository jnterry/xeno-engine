////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Platform unix implementation of DynamicLibrary
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_DYNAMICLIBRARY_UNIX_CPP
#define XEN_KERNEL_DYNAMICLIBRARY_UNIX_CPP

#include "DynamicLibrary.hxx"

#include <xen/core/memory/Allocator.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>
#include <xen/core/StringBuffer.hpp>

#include <dlfcn.h>

#include <cstring>
#include <cstdio>

namespace xke {

  char* resolveDynamicLibrary(xen::Allocator& alloc, const char* name){
		XenTempStringBuffer(strbuf, 4096, name);
		xen::String original = strbuf;

		if(xen::pathExists(strbuf)){ goto alloc_string; }

		xen::appendString(strbuf, ".so");
		if(xen::pathExists(strbuf)){ goto alloc_string; }

		xen::resetStringBuffer(strbuf, original);
		xen::appendString(strbuf, "d.so");
		if(xen::pathExists(strbuf)){ goto alloc_string; }

		if(!xen::startsWith(strbuf, "lib")){
			xen::resetStringBuffer(strbuf, original);
			xen::prependString(strbuf, "lib");
			return resolveDynamicLibrary(alloc, strbuf);
		}
		return nullptr;

	  alloc_string: {
			u64 strlen = xen::stringLength(strbuf);
			char* result = (char*)alloc.allocate(strlen);
			memcpy(result, (const char*)strbuf, strlen);
			return result;
		}
	}

	DynamicLibrary* loadDynamicLibrary(xen::Allocator& alloc, const char* path){

		XenTempStringBuffer(strbuf, 4096, path);
		xen::appendStringf(strbuf, "-%lu", xen::asNanoseconds<u64>(xen::getTimestamp()));
		xen::copyFile(path, strbuf);

		void* result = dlopen(strbuf, RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND);

		if(result == nullptr){
			// :TODO: log
			printf("Error loading dynamic library at '%s', error: %s\n", path, dlerror());
		}

		// we can delete the file even though our process has it open, the number of
		// references to the inode will be decremented by 1, but since our process
		// has it open the file will not be deleted, space on disk will be freed
		// when process exits or unload is called
		xen::deleteFile(strbuf);

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
