#include <cstdio>

#include "game.hpp"

int main(){
	printf("Hello world!\n");

	xen::KernelSettings settings;
	settings.loop = &mainLoop;

	xen::startKernel(settings);

	printf("End of main\n");
}
