#include <xen/kernel/Kernel.hpp>

#include <cstdio>

int main(int argc, const char** argv){
	if(argc < 2){
		printf("Error: usage: %s <GAME-MODULE>", argv[0]);
		return 1;
	}

	xen::KernelSettings settings;
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::createKernel(settings);

	xen::loadModule(kernel, "xen-module-sren-rasterize", nullptr);
	xen::loadModule(kernel, argv[1], nullptr);
	xen::startKernel(kernel);
}
