#include <cstdio>

#include "game.hpp"

#include <xen/core/memory.hpp>
#include <xen/core/File.hpp>

int main(){
	xen::KernelSettings settings = {0};
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::initKernel(settings);

	GameModuleParams game_params;
	game_params.increment_delay = 100;

  auto module_game = xen::loadModule("dynamic-reload-game", &game_params);

	printf("Loaded game module: %p\n", module_game);

	xen::startKernel(kernel);

	printf("End of main\n");
}
