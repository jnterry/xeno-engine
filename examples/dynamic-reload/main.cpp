#include <cstdio>

#include "game.hpp"

#include <xen/kernel/DynamicLibrary.hpp>
#include <xen/core/memory.hpp>

int main(){
	xen::KernelSettings settings;
	xen::Kernel* kernel = xen::createKernel(settings);

	xen::AllocatorMalloc alloc;

	xen::DynamicLibrary* game_lib = xen::loadDynamicLibrary(alloc, "../lib/libdynamic-reload-gamed");
	printf("Loaded lib at: %p\n", (void*)game_lib);

	Game* game = (Game*)xen::getDynamicLibrarySymbol(game_lib, "game");
	printf("Loaded symbol at: %p\n", (void*)game);

	xen::startKernel(kernel, game->tick);

	printf("End of main\n");
}
