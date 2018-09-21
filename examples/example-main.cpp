#include <xen/kernel/Kernel.hpp>

int main(int argc, const char** argv){
	xen::KernelSettings settings;
	settings.hot_reload_modules = true;
	xen::Kernel& kernel = xen::createKernel(settings);

	xen::loadModule(kernel, argv[1], nullptr);
	xen::startKernel(kernel);
}
