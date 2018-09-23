#include <chrono>
#include <thread>

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>
#include <xen/core/String.hpp>

#include "game.hpp"

struct GameState {
	u64 value;
	int increment_delay;
};

GameState* global_state = nullptr;

void tick(xen::Kernel& kernel, const xen::TickContext& cntx){
	printf("Start of game main loop, tick: %5lu, dt: %10f, time: %10f\n",
	       cntx.tick,
	       xen::asSeconds<real>(cntx.dt),
	       xen::asSeconds<real>(cntx.time)
	      );

	// processing
	printf("Game module is doing some processing...\n");
	printf("Value is: %lu\n", global_state->value);
	std::this_thread::sleep_for(std::chrono::milliseconds(16));

	if(cntx.tick % global_state->increment_delay == 0){
		++global_state->value;
	}

	//if(cntx.time > xen::seconds(3)){
	//if(cntx.tick > 5){
	//	return false;
	//}

	printf("End main loop\n");
}

void* init(xen::Kernel& kernel, const void* params){
	global_state = (GameState*)xen::allocate(kernel, sizeof(GameState));
	global_state->value = 10;
	global_state->increment_delay = ((const GameModuleParams*)params)->increment_delay;
	return global_state;
}

void* load(xen::Kernel& kernel, void* data, const void* params){
	global_state = (GameState*)data;
	return (void*)true;
}

void shutdown(xen::Kernel& kernel){
	printf("Shutting down game module\n");
}

xen::Module exported_xen_module = {
	xen::hash("game"),
	&init,
	&shutdown,
	&load,
	&tick,
};
