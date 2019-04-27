////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Unix implementation of platform specific kernel functionality
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_UNIX_CPP
#define XEN_KERNEL_KERNEL_UNIX_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>
#include <xen/core/StringBuffer.hpp>
#include <xen/kernel/log.hpp>

#include <dlfcn.h>

#include <cstring>
#include <cstdio>

// sigsegv handler includes
#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <cxxabi.h>

namespace {
	char* resolveDynamicLibrary(xen::ArenaLinear& arena, const char* name){
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
			return resolveDynamicLibrary(arena, strbuf);
		}
		return nullptr;

	  alloc_string:
		return xen::pushString(arena, strbuf.start);
	}

	xen::Module* doLoad(xke::ModuleSource* msrc){
		msrc->lib_modification_time = xen::getPathModificationTime(msrc->lib_path);

		XenTempStringBuffer(path_strbuf, 4096, msrc->lib_path);
		xen::appendStringf(path_strbuf, "-%lu", xen::asNanoseconds<u64>(xen::getTimestamp()));

		const char* path = path_strbuf;
		if(!xen::copyFile(msrc->lib_path, path_strbuf)){
			XenLogWarn("Failed to copy library before opening: %s", msrc->lib_path);
			path = msrc->lib_path;
		}

		msrc->lib_handle = dlopen(path, RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND);

		if(msrc->lib_handle == nullptr){
			XenLogError("Error loading dynamic library %s, error: %s", msrc->lib_path, dlerror());
			return nullptr;
		}

		if(msrc->lib_path != path){
			// we can delete the file even though our process has it open, the number of
			// references to the inode will be decremented by 1, but since our process
			// has it open the file will not be deleted, space on disk will be freed
			// when process exits or unload is called
			xen::deleteFile(path);
		}

		xen::Module* result = (xen::Module*)dlsym(msrc->lib_handle, "exported_xen_module");
		if(result == nullptr){
			XenLogError("Failed to load symbol 'exported_xen_module' from dynamic library %s, error: %s",
			            msrc->lib_path, dlerror()
			            );
			return nullptr;
		}

		return result;
	}
}

xen::Module* xke::platformLoadModule(const char* name, xke::ModuleSource* msrc){
	const char* lib_path = resolveDynamicLibrary(xke::kernel.system_arena, name);
	if(lib_path == nullptr){
		XenLogError("Failed to find library file for module: %s", name);
		return nullptr;
	}

	msrc->lib_path = lib_path;

	return doLoad(msrc);
}


xen::Module* xke::platformReloadModuleIfChanged(xke::ModuleSource* msrc){
	xen::DateTime mod_time = xen::getPathModificationTime(msrc->lib_path);

	if(mod_time <= msrc->lib_modification_time){
		return nullptr;
	}

	xen::DateTime now = xen::getLocalTime();

	if(now - mod_time < xen::seconds(1.5f)){
		// :TODO: this is a nasty hack - issue is that the linker will
		// truncate the file, then begin writing to it. Initial truncation
		// changes modification time, so we may start trying to load the
		// module before the linker has finished writing it. This says that
		// we should only load the module if it changed AND that was at least
		// 1 second ago. If linker takes longer than 1 second this will blow
		// up
		return nullptr;
	}

	XenLogInfo("Reloading modified module: %s", msrc->lib_path);
	xke::platformUnloadModule(msrc);
	return doLoad(msrc);
}


bool xke::platformUnloadModule(ModuleSource* msrc){
	if(dlclose(msrc->lib_handle) != 0){
		XenLogWarn("Failed to close dynamic library %s, error: %s",
		            msrc->lib_path, dlerror()
		           );
		return false;
	}
	return true;
}


////////////////////////////////////////////////////////////////////////////////

namespace {
	void sigsegvHandler(int sig) {
		void* array[256];
		size_t size;

		// get void*'s for all entries on the stack
		size = backtrace(array, 256);

		// print out all the frames to stderr
		fprintf(stderr, "Error: signal SIGSEGV\n");
		if(size == 0){
			fprintf(stderr, "Stack empty, corrupted?\n");
		}

		char** symbollist = backtrace_symbols(array, size);
		size_t func_name_size = 512;
		char* func_name = (char*)malloc(func_name_size);

		// Iterate over list of stack entries and print, skip this function
		// and the sigsegv handler code
		for(u32 i = 2; i < size; ++i){
			// Find paranthses and +address offset surrounding the mangled name
			// Eg: ./module(function+0xff) [0x12345678]
			char* begin_name   = nullptr;
			char* begin_offset = nullptr;
			char* end_offset   = nullptr;
			for(char* p = symbollist[i]; *p != '\0'; ++p){
				switch(*p){
				case '(': begin_name   = p; break;
				case '+': begin_offset = p; break;
				case ')': end_offset   = p; break;
				default: break;
				}
			}
			if(begin_name && begin_offset && end_offset && begin_name < begin_offset){
				*begin_name++   = '\0';
				*begin_offset++ = '\0';
				*end_offset     = '\0';

				int status;
				char* ret = abi::__cxa_demangle(begin_name, func_name, &func_name_size, &status);
				if(status == 0){
					func_name = ret;
				}
				fprintf(stderr, "%2i : %s+%s\n", i-2, func_name, begin_offset);
			} else {
				fprintf(stderr, "%2i : %s\n", i-2, symbollist[i]);
			}
		}

		//backtrace_symbols_fd(array, size, STDERR_FILENO);
		exit(1);
	}

	void sigintHandler(int sig){
		if(xke::kernel.state == xke::Kernel::RUNNING){
			XenLogInfo("SIGINT intercepted, requesting kernel shutdown");
			xen::requestKernelShutdown();
		} else {
			printf("SIGINT intercepted but kernel not running, force quitting\n");
			exit(0);
		}
	}
}

void xke::platformRegisterSignalHandlers(){
	signal(SIGSEGV, sigsegvHandler);
	signal(SIGINT,  sigintHandler);
}


#endif
