////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Platform windows implementation of DynamicLibrary
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_DYNAMICLIBRARY_WIN_CPP
#define XEN_KERNEL_DYNAMICLIBRARY_WIN_CPP

#include "DynamicLibrary.hxx"

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>
#include <xen/core/StringBuffer.hpp>
#include <xen/kernel/log.hpp>

#include <cstring>
#include <cstdio>

#include <xen/windows_header.hxx>

namespace xke {

  char* resolveDynamicLibrary(const char* name){
		XenTempStringBuffer(strbuf, 4096, name);
		xen::String original = strbuf;

		if(xen::pathExists(strbuf)){ goto alloc_string; }

		xen::appendString(strbuf, ".dll");
		if(xen::pathExists(strbuf)){ goto alloc_string; }

		xen::resetStringBuffer(strbuf, original);
		xen::appendString(strbuf, "d.dll");
		if(xen::pathExists(strbuf)){ goto alloc_string; }

		return nullptr;

	  alloc_string:
		return xen::pushString(xen::getThreadScratchSpace(), strbuf.start);
	}

	DynamicLibrary* loadDynamicLibrary(xen::Allocator& alloc, const char* path){

		XenTempStringBuffer(strbuf, 4096, path);
		xen::appendStringf(strbuf, "-%lu", xen::asNanoseconds<u64>(xen::getTimestamp()));
		xen::copyFile(path, strbuf);

		HMODULE hmodule = LoadLibrary(strbuf);

		if(hmodule == NULL){
			XenLogError("Error loading dynamic library '%s', error code: %u", path, GetLastError());
		}

		xen::deleteFile(strbuf);

		DynamicLibrary* result = (DynamicLibrary*)alloc.allocate(sizeof(DynamicLibrary), alignof(DynamicLibrary));
		result->hmodule = hmodule;
		return result;
	}

	void unloadDynamicLibrary(xen::Allocator& alloc, DynamicLibrary* lib){
		if(lib != nullptr){
			if(lib->hmodule != NULL){
				if(!FreeLibrary(lib->hmodule)){
					XenLogError("Failed to close dynamic library, error code: %u", GetLastError());
				}
			}
			alloc.deallocate(lib);
		}
	}

	void* getDynamicLibrarySymbol(DynamicLibrary* lib, const char* name){
	  FARPROC result = GetProcAddress(lib->hmodule, name);
	  if(result == NULL){
		  XenLogError("Failed to load symbol from dynamic library, error code: %u", GetLastError());
	  }
	  return result;
	}
}

#endif
