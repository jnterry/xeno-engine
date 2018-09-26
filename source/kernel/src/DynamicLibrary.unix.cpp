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

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>
#include <xen/core/StringBuffer.hpp>
#include <xen/kernel/log.hpp>

#include <dlfcn.h>

#include <cstring>
#include <cstdio>

namespace xke {

  char* resolveDynamicLibrary(const char* name){
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
			return resolveDynamicLibrary(strbuf);
		}
		return nullptr;

	  alloc_string:
		return xen::pushString(xen::getThreadScratchSpace(), strbuf.start);
	}

	DynamicLibrary* loadDynamicLibrary(xen::Allocator& alloc, const char* path){

		XenTempStringBuffer(strbuf, 4096, path);
		xen::appendStringf(strbuf, "-%lu", xen::asNanoseconds<u64>(xen::getTimestamp()));
		xen::copyFile(path, strbuf);

		void* result = dlopen(strbuf, RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND);

		if(result == nullptr){
			XenLogError("Error loading dynamic library '%s', error: %s", path, dlerror());
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
		  XenLogError("Failed to close dynamic library: %s", dlerror());
		}
	}

	void* getDynamicLibrarySymbol(DynamicLibrary* lib, const char* name){
	  void* result = dlsym(lib, name);
	  if(result == nullptr){
		  XenLogError("Failed to load symbol from dynamic library: %s", dlerror());
	  }

	  return result;
	}

}

#endif
