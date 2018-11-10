////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of windows specific kernel functionality
///
/// \ingroup kerenl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_WIN_CPP
#define XEN_KERNEL_KERNEL_WIN_CPP

#include "Kernel.hxx"

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>
#include <xen/core/StringBuffer.hpp>
#include <xen/kernel/log.hpp>

#include <xen/windows_header.hxx>

namespace {
	typedef void*(*ProcGetExportedModule)(XenKernelSyscalls*);

	XenKernelSyscalls syscalls = {
		&xen::loadModule,
		&xen::getModuleApi,
		&xen::kernelAlloc,
		&xen::kernelFree,
		&xen::requestKernelShutdown,
		&xen::logv,
		&xen::createTickWorkGroup,
		&xen::pushTickWork,
		&xen::pushTickWork,
		&xen::waitForTickWork,
		&xen::getThreadIndex,
		&xen::getThreadCount,
		&xen::getThreadScratchSpace
	};

	char* resolveDynamicLibrary(xen::ArenaLinear& arena, const char* name){
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
		return xen::pushString(arena, strbuf.start);
	}

	xen::Module* doLoad(xke::ModuleSource* msrc){
		msrc->lib_modification_time = xen::getPathModificationTime(msrc->lib_path);

		msrc->lib_loaded_suffix = xen::asNanoseconds<u64>(xen::getTimestamp());
		XenTempStringBuffer(path_strbuf, 4096, msrc->lib_path);
		xen::appendStringf(path_strbuf, "-%lu", msrc->lib_loaded_suffix);

		const char* path = path_strbuf;
		if(!xen::copyFile(msrc->lib_path, path_strbuf)){
			XenLogWarn("Failed to copy library before opening: %s", msrc->lib_path);
			path = msrc->lib_path;
		}

		msrc->lib_handle = LoadLibrary(path);

		if(msrc->lib_handle == nullptr){
			XenLogError("Error loading dynamic library %s, error: %s",
			            msrc->lib_path, GetLastError());
			return nullptr;
		}

		if(msrc->lib_path != path){
			// we can delete the file even though our process has it open, the number of
			// references to the inode will be decremented by 1, but since our process
			// has it open the file will not be deleted, space on disk will be freed
			// when process exits or unload is called

		}

		ProcGetExportedModule proc = (ProcGetExportedModule)GetProcAddress(msrc->lib_handle, "getExportedXenModule");

		if(proc == nullptr){
			XenLogError("Failed to load symbol 'getExportedXenModule' from dynamic library %s, error: %s",
			            msrc->lib_path, GetLastError());
		}

		xen::Module* result = (xen::Module*)proc(&syscalls);
		if(result == nullptr){
			XenLogError("Procedure 'getExportedXenModule' in module '%s' returned nullptr",
			            msrc->lib_path
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
	if(msrc == nullptr || msrc->lib_handle == NULL){
		return true;
	}

	XenTempStringBuffer(path_strbuf, 4096, msrc->lib_path);
	xen::appendStringf(path_strbuf, "-%lu", msrc->lib_loaded_suffix);
	xen::deleteFile(path_strbuf);

	if(!FreeLibrary(msrc->lib_handle)){
		XenLogError("Failed to close dynamic library, error code: %u", GetLastError());
		return false;
	}

	return true;
}

////////////////////////////////////////////////////////////////////////////////

void xke::platformRegisterSignalHandlers(){
	printf("Platform signal handlers are not supported on Windows\n");
}

#endif
