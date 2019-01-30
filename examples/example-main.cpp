#include <xen/kernel/Kernel.hpp>
#include <xen/kernel/log.hpp>

#include <cstring>
#include <cstdio>
#include <cstdlib>


xen::StringHash loadGraphicsModule(const char* cli_arg){
	if(strcmp(cli_arg, "gl") == 0){
		return xen::loadModule("xen-module-gl");
	} else if (strcmp(cli_arg, "rasterize") == 0){
		return xen::loadModule("xen-module-sren-rasterize");
	} else if (strcmp(cli_arg, "raytrace") == 0){
		return xen::loadModule("xen-module-sren-raytrace");
	} else if (strcmp(cli_arg, "atom") == 0){
		return  xen::loadModule("xen-module-sren-atom");
	} else {
		XenLogFatal("Invalid graphics module name: %s", cli_arg);
		exit(2);
	}
}

int main(int argc, const char** argv){
	if(argc < 3){
		fprintf(stderr, "Error: usage: %s <GAME-MODULE> <GRAPHICS-MODULE>\n", argv[0]);
		return 1;
	}

	xen::KernelSettings settings = {0};
	settings.hot_reload_modules = true;
	settings.print_tick_rate    = true;

	if(!xen::initKernel(settings)){
		fprintf(stderr, "Failed to initialize kernel\n");
		return 2;
	}

	if(!xen::loadModule("xen-module-window")){
		XenLogFatal("Failed to load window module");
		return 3;
	}

	if(!loadGraphicsModule(argv[2])){
		XenLogFatal("Failed to load graphics module");
		return 4;
	}


	if(!xen::loadModule(argv[1])){
		XenLogFatal("Failed to load game module: %s", argv[1]);
		return 5;
	}

	xen::startKernel();

	return 0;
}
