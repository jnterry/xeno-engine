#include <cstdio>

#include "game.hpp"

#include <xen/core/memory.hpp>
#include <xen/core/File.hpp>

int main(){
	xen::KernelSettings settings;
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::createKernel(settings);

	GameApi* module_game = (GameApi*)xen::loadModule(kernel, "dynamic-reload-game");

	printf("Loaded game module: %p\n", (void*)module_game);

	xen::startKernel(kernel, module_game->tick);

	printf("End of main\n");
}
