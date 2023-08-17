////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of types defined in Kernel.hpp
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_CONTEXT_CPP
#define XEN_KERNEL_CONTEXT_CPP

#include <xen/kernel/Kernel.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/File.hpp>
#include <xen/core/time.hpp>
#include <xen/kernel/log.hpp>

#include <xen/config.hpp>

#include <utility>
#include <new>

#include "Kernel.hxx"
#include "threads.hxx"
#include "log.hxx"

xke::Kernel xke::kernel;

namespace {

	bool doModuleInit(xke::LoadedModule* lmod){
		if(lmod->data == nullptr){
			XenLogInfo("Initializing module: %s", lmod->lib_path);
			if(lmod->module->initialize == nullptr){
				lmod->data = nullptr;
			} else {
				lmod->data = lmod->module->initialize(lmod->params);
				if(lmod->data == nullptr){
					XenLogError("Failed to initizalise module '%s'", lmod->lib_path);
					return false;
				}
			}
		}

		XenLogInfo("Loading module's API: %s", lmod->lib_path);
		if(lmod->module->load == nullptr){
			lmod->api = (void*)true;
		} else {
			lmod->api = lmod->module->load(lmod->data, lmod->params);
			if(lmod->api == nullptr){
				XenLogError("Module's load function returned nullptr, module: '%s'", lmod->lib_path);
				return false;
			}
		}

		return true;
	}

	void reloadModifiedKernelModules(){
		for(xke::LoadedModule* lmod = xke::kernel.module_head;
		    lmod != nullptr;
		    lmod = lmod->next
		   ){

			xen::Module* module = xke::platformReloadModuleIfChanged(lmod);

			if(module != nullptr){
				lmod->module = module;
				doModuleInit(lmod);
			}
		}
	}
}

bool xen::initKernel(const xen::KernelSettings& settings){
	XenAssert(xke::kernel.state == xke::Kernel::UNINITIALIZED,
	          "Expected kernel to be initialised only once"
	         );

	xke::platformRegisterSignalHandlers();

	constexpr u32 SYSTEM_ARENA_SIZE = xen::kilobytes(16);

	xke::kernel.root_allocator = new xen::AllocatorMalloc();

	xke::kernel.system_arena = xen::ArenaLinear(
		xke::kernel.root_allocator->allocate(SYSTEM_ARENA_SIZE),
		SYSTEM_ARENA_SIZE
	);

	xen::copyBytes(&settings, &xke::kernel.settings, sizeof(xen::KernelSettings));

	xke::kernel.modules = xen::createArenaPool<xke::LoadedModule>(xke::kernel.system_arena, 128);

	if(!xke::initLogSubsystem()){
		printf("Error occured while initializing log subsystem\n");
		xen::destroyArenaLinear(*xke::kernel.root_allocator, xke::kernel.system_arena);
		delete xke::kernel.root_allocator;
		return false;
	}

	if(!xke::initThreadSubsystem()){
	  XenLogError("Error occured while initializing thread subsystem of kernel");
		xen::destroyArenaLinear(*xke::kernel.root_allocator, xke::kernel.system_arena);
		delete xke::kernel.root_allocator;
		return false;
	}

	XenLogDone("Finished kernel init, used %lu of %lu system arena bytes",
	           xen::ptrDiff(xke::kernel.system_arena.start, xke::kernel.system_arena.next_byte),
	           xen::ptrDiff(xke::kernel.system_arena.start, xke::kernel.system_arena.end)
	          );

	xke::kernel.state = xke::Kernel::INITIALIZED;

	return true;
}

xen::StringHash xen::loadModule(
	const char* name, const void* params
){
	XenAssert(xke::kernel.state >= xke::Kernel::INITIALIZED,
		      "Expected initKernel to be called before loadModule");

	xke::LoadedModule* lmod = xen::reserveType<xke::LoadedModule>(xke::kernel.modules);
	xen::clearToZero(lmod);

	if(lmod == nullptr){
		XenLogError("Failed to load module as max number of loaded modules has been reached");
		goto cleanup;
	}

	lmod->module = xke::platformLoadModule(name, lmod);
	if(lmod->module == nullptr){
		goto cleanup;
	}

	lmod->params     = params;
	lmod->next       = xke::kernel.module_head;
	xke::kernel.module_head = lmod;

	if(!doModuleInit(lmod)){
		goto cleanup;
	}

	XenLogDone("Finished initializing and loading module: %s", name);
	return lmod->module->type_hash;

	cleanup: {
		XenLogWarn("Cleaning up from failed module load: %s", name);
		if(lmod == nullptr){ return 0; }

		if(lmod->module) {
			xke::platformUnloadModule(lmod);
		}
		xen::freeType<xke::LoadedModule>(xke::kernel.modules, lmod);

		return 0;
	}
}

void xen::startKernel(){
	XenAssert(xke::kernel.state == xke::Kernel::INITIALIZED,
	          "Expected kernel to be initialised but not started");

	XenAssert(0 == xen::getThreadIndex(),
	          "Expected master thread to call startKernel");

	xen::TickContext cntx = {0};

	xen::Stopwatch timer;
	xen::Duration last_time = timer.getElapsedTime();

	xen::Duration last_tick_rate_print = last_time;
	u64           last_tick_count      = 0;

	xke::kernel.state = xke::Kernel::RUNNING;

	XenLogInfo("Kernel is beginning main loop");
	while (!xke::kernel.stop_requested) {
		xke::preTickThreadSubsystem();

		cntx.time = timer.getElapsedTime();
		cntx.dt = cntx.time - last_time;

		if(xke::kernel.settings.hot_reload_modules){
			reloadModifiedKernelModules();
		}

		if(xke::kernel.settings.print_tick_rate &&
		   cntx.time - last_tick_rate_print > xen::seconds(0.5f)){
		  XenLogInfo("Tick Rate: %f",
		             (real)(cntx.tick - last_tick_count) /
		             xen::asSeconds<real>(cntx.time - last_tick_rate_print)
		            );
			last_tick_rate_print = cntx.time;
			last_tick_count      = cntx.tick;
		}

		for(xke::LoadedModule* lmod = xke::kernel.module_head;
		    lmod != nullptr;
		    lmod = lmod->next
		    ){


			if(lmod->module->tick != nullptr){
				cntx.data   = lmod->data;
				cntx.params = lmod->params;
				lmod->module->tick(cntx);
			}
		}

		// Ensure that all tick work is completed
		xen::waitForTickWork(0);

		++cntx.tick;
		last_time = cntx.time;
	}
	XenLogInfo("Kernel has left main loop, doing cleanup...");

	xke::kernel.state = xke::Kernel::STOPPED;

	xke::LoadedModule* lmod = xke::kernel.module_head;
	while(lmod != nullptr){
		if(lmod->module->unload != nullptr){
			lmod->module->unload(lmod->data, lmod->params);
		}
		XenLogInfo("Doing shutdown for module: %s", lmod->lib_path);
		lmod->module->shutdown(lmod->data, lmod->params);
		lmod = lmod->next;
	}

	// free resources, check for memory leaks, etc
	xke::stopThreadSubsystem();

	xke::kernel.state = xke::Kernel::SHUTDOWN;
	XenLogDone("Kernel has finished cleanup");
}

void* xen::getModuleApi(xen::StringHash hash){
	for(xke::LoadedModule* cur = xke::kernel.module_head;
	    cur != nullptr;
	    cur = cur->next
	    ){
		if(cur->module->type_hash == hash){
			return cur->api;
		}
	}
	return nullptr;
}

void* xen::kernelAlloc(u32 size, u32 align){
	// :TODO: we ideally want an allocator per module so we can debug memory
	// leaks etc
	return xke::kernel.root_allocator->allocate(size, align);
}

void xen::kernelFree(void* data){
	return xke::kernel.root_allocator->deallocate(data);
}

void xen::requestKernelShutdown(){
	xke::kernel.stop_requested = true;
}

#include <xen/config.hpp>
#ifdef XEN_OS_WINDOWS
	#include "Kernel.win.cpp"
#elif defined XEN_OS_UNIX
	#include "Kernel.unix.cpp"
#else
  #error "Kernel is not implemented on this platform"
#endif

#endif
