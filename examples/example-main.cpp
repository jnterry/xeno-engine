#include <xen/kernel/Kernel.hpp>

#include <cstring>
#include <cstdio>

int main(int argc, const char** argv){
	if(argc < 3){
		printf("Error: usage: %s <GAME-MODULE> <GRAPHICS-MODULE>\n", argv[0]);
		return 1;
	}

	xen::KernelSettings settings;
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::createKernel(settings);

	if(strcmp(argv[2], "gl") == 0){
		xen::loadModule(kernel, "xen-module-gl", nullptr);
	} else if (strcmp(argv[2], "rasterize") == 0){
		xen::loadModule(kernel, "xen-module-sren-rasterize", nullptr);
	} else {
		printf("Invalid graphics module name: %s\n", argv[2]);
		return 2;
	}

	xen::loadModule(kernel, argv[1], nullptr);
	xen::startKernel(kernel);
}
