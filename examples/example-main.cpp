#include <xen/kernel/Kernel.hpp>

#include <cstring>
#include <cstdio>


void loadGraphicsModule(xen::Kernel& kernel, const char* cli_arg){
	if(strcmp(cli_arg, "gl") == 0){
		xen::loadModule("xen-module-gl");
	} else if (strcmp(cli_arg, "rasterize") == 0){
		xen::loadModule("xen-module-sren-rasterize");
	} else if (strcmp(cli_arg, "raytrace") == 0){
		xen::loadModule("xen-module-sren-raytrace");
	} else if (strcmp(cli_arg, "atom") == 0){
		xen::loadModule("xen-module-sren-atom");
	} else {
		printf("Invalid graphics module name: %s\n", cli_arg);
		exit(2);
	}
}

int main(int argc, const char** argv){
	if(argc < 3){
		printf("Error: usage: %s <GAME-MODULE> <GRAPHICS-MODULE>\n", argv[0]);
		return 1;
	}

	xen::KernelSettings settings = {0};
	settings.hot_reload_modules = true;
	settings.print_tick_rate    = true;
	xen::Kernel& kernel = xen::initKernel(settings);

	loadGraphicsModule(kernel, argv[2]);
	xen::loadModule(argv[1]);
	xen::startKernel(kernel);

	return 0;
}
