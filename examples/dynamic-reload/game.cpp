#include <chrono>
#include <thread>

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>

#include "game.hpp"

bool mainLoop(const xen::TickContext& cntx){
	printf("Start of game main loop, tick: %5lu, dt: %10f, time: %10f\n",
	       cntx.tick,
	       xen::asSeconds<real>(cntx.dt),
	       xen::asSeconds<real>(cntx.time)
	      );

	// processing
	printf("Game module is doing some processing...\n");
	std::this_thread::sleep_for(std::chrono::milliseconds(16));

	//if(cntx.time > xen::seconds(3)){
	if(cntx.tick > 5){
		return false;
	}

	printf("End main loop\n");
	return true;
}

bool init(xen::Kernel& kernel){
	printf("Initialising game module\n");
	return true;
}

void shutdown(xen::Kernel& kernel){
	printf("Shutting down game module\n");
}

Game::Game()
	: tick(&mainLoop)
{
	this->init     = &::init;
	this->shutdown = &::shutdown;
}

Game exported_xen_module = Game();
