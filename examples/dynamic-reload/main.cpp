#include <cstdio>

#include "game.hpp"

#include <xen/core/memory.hpp>
#include <xen/core/File.hpp>

int main(){
	xen::KernelSettings settings;
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::createKernel(settings);

	GameModuleParams game_params;
	game_params.increment_delay = 100;

	GameApi* module_game = (GameApi*)xen::loadModule(kernel, "dynamic-reload-game", &game_params);

	printf("Loaded game module: %p\n", (void*)module_game);

	xen::startKernel(kernel);

	printf("End of main\n");
}
