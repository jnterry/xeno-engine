#include <xen/kernel/Kernel.hpp>

#include <cstring>
#include <cstdio>


const char* loadGraphicsModule(xen::Kernel& kernel, const char* cli_arg){
	if(strcmp(cli_arg, "gl") == 0){
		xen::loadModule(kernel, "xen-module-gl");
	} else if (strcmp(cli_arg, "rasterize") == 0){
		xen::loadModule(kernel, "xen-module-sren-rasterize");
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

	xen::KernelSettings settings;
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::createKernel(settings);

	loadGraphicsModule(kernel, argv[2]);
	xen::loadModule(kernel, argv[1]);
	xen::startKernel(kernel);
}
