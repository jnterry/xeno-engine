////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains windows specific macro defintions for modules
///
/// In windows loaded DLLs cannot call functions linked into the executable
/// (unless they are exported with __declspec(dllexprt) and GetProcAddress is
/// used from the DLL to get a function pointer to the proc in the executable)
///
/// We cannot link the individual modules to the xen-kernel library as global
/// kernel state would be duplicated in each module.
///
/// It is convientent to be able to do kernel system calls as if they were
/// regular function calls (as indeed they are under unix systems where
/// the dynamic linker at run time patches up the call address automatically
/// so the loaded ".so" libraries can call functions in the executable).
/// This would not be possible if we used the GetProcAddress method, as the
/// kernel headers would have to declare function pointers rather than actual
/// functions such that we can fill them in at run time with GetProcAddress.
/// Instead, in order to avoid undefined reference errors in the modules we
/// provide a stub implementation of every kernel function as part of the
/// XenDeclareModule macro. These stub functions simply call a function pointer
/// that the kernel passes to the function which returns the xen::Module
/// instance to the kernel
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_MODULE_WIN_HPP
#define XEN_KERNEL_MODULE_WIN_HPP

#include <xen/core/String.hpp>
#include <xen/kernel/threads.hpp>
#include <xen/kernel/log.hpp>


// Forward declerations required for XenKernelSyscalls
namespace xen {
	struct KernelSettings;
	struct ArenaLinear;
	typedef unsigned int TickWorkHandle;
}

struct XenKernelSyscalls {
	xen::StringHash    (*loadModule           )(const char*, const void*);
	void*              (*getModuleApi         )(u64);
	void*              (*kernelAlloc          )(u32, u32);
	void               (*kernelFree           )(void*);
	void               (*requestKernelShutdown)();
	void               (*logv                 )(u08, const char*, u32, const char*, va_list);
	xen::TickWorkHandle(*createTickWorkGroup  )();
	xen::TickWorkHandle(*pushTickWork         )(xen::TickWorkFunction, void*, u64, xen::TickWorkHandle);
	xen::TickWorkHandle(*pushTickWorkSimple   )(xen::SimpleTickWorkFunction, void*, u64, xen::TickWorkHandle);
	void               (*waitForTickWork      )(xen::TickWorkHandle work);
	xen::ThreadIndex   (*getThreadIndex       )();
	xen::ThreadIndex   (*getThreadCount       )();
	xen::ArenaLinear&  (*getThreadScratchSpace)();
};

extern XenKernelSyscalls _xen_kernel_syscalls;

#define XenDeclareModule(NAME, INIT, SHUTDOWN, LOAD, UNLOAD, TICK) \
	::xen::Module exported_xen_module = { \
		xen::hash(NAME), \
		INIT, SHUTDOWN, \
		LOAD, UNLOAD, \
		TICK \
	}; \
	XenKernelSyscalls _xen_kernel_syscalls = {0}; \
	extern "C" { \
		__declspec(dllexport) void* getExportedXenModule(XenKernelSyscalls* syscalls){ \
			_xen_kernel_syscalls = *syscalls; \
			return &exported_xen_module; \
		} \
	} \
	xen::StringHash xen::loadModule(const char* name, const void* params){ \
		return _xen_kernel_syscalls.loadModule(name, params); \
	} \
	void* xen::getModuleApi(u64 type_hash){ \
		return _xen_kernel_syscalls.getModuleApi(type_hash); \
	} \
	void* xen::kernelAlloc(u32 size, u32 align) { \
		return _xen_kernel_syscalls.kernelAlloc(size, align); \
	} \
	void xen::kernelFree(void* data) { \
		return _xen_kernel_syscalls.kernelFree(data); \
	} \
	void xen::requestKernelShutdown() { \
		return _xen_kernel_syscalls.requestKernelShutdown(); \
	} \
	void xen::logv(u08 level, const char* file, u32 line, const char* msg_format, va_list args) { \
		return _xen_kernel_syscalls.logv(level, file, line, msg_format, args); \
	} \
	xen::TickWorkHandle xen::createTickWorkGroup(){ \
		return _xen_kernel_syscalls.createTickWorkGroup(); \
	} \
	xen::TickWorkHandle xen::pushTickWork(xen::TickWorkFunction work, void* data, u64 data_size, xen::TickWorkHandle group) { \
		return _xen_kernel_syscalls.pushTickWork(work, data, data_size, group); \
	} \
	xen::TickWorkHandle xen::pushTickWork(xen::SimpleTickWorkFunction work, void* data, u64 data_size, xen::TickWorkHandle group) { \
		return _xen_kernel_syscalls.pushTickWorkSimple(work, data, data_size, group); \
	} \
	void xen::waitForTickWork(xen::TickWorkHandle work) { \
		return _xen_kernel_syscalls.waitForTickWork(work); \
	} \
	xen::ThreadIndex xen::getThreadIndex() { \
		return _xen_kernel_syscalls.getThreadIndex(); \
	} \
	xen::ThreadIndex xen::getThreadCount() { \
		return _xen_kernel_syscalls.getThreadCount(); \
	} \
	xen::ArenaLinear& xen::getThreadScratchSpace() { \
		return _xen_kernel_syscalls.getThreadScratchSpace(); \
	}

#endif
