#include <chrono>
#include <thread>

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>

#include "game.hpp"

bool mainLoop(const xen::Context& cntx){
	printf("Start of game main loop, tick: %5lu, dt: %10f, time: %10f\n",
	       cntx.tick,
	       xen::asSeconds<real>(cntx.dt),
	       xen::asSeconds<real>(cntx.time)
	      );

	// processing
	std::this_thread::sleep_for(std::chrono::milliseconds(16));

	if(cntx.time > xen::seconds(3)){
		return false;
	}

	printf("End main loop\n");
	return true;
}

Game game = { mainLoop };
