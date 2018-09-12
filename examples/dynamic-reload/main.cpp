#include <cstdio>

#include "game.hpp"

#include <xen/core/memory.hpp>

int main(){
	xen::KernelSettings settings;
	xen::Kernel& kernel = xen::createKernel(settings);

	Game* module_game = (Game*)xen::loadModule(kernel, "../lib/libdynamic-reload-gamed");

	printf("Loaded game module: %p\n", (void*)module_game);

	xen::startKernel(kernel, module_game->tick);

	printf("End of main\n");
}
