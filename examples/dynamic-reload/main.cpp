#include <cstdio>

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>

#include <chrono>
#include <thread>

namespace xen {

	struct Context;
	typedef bool (*MainLoopFunction)(const Context& cntx);

	struct EngineSettings {
		/// \brief Function which represents all computation that should occur
		/// in one frame of the application
		MainLoopFunction loop;
	};

	struct Context {
		/// \brief Time since the engine started
		xen::Duration time;

		/// \brief Delta time since the last call to loop
		xen::Duration dt;

		/// \brief Integer which is incremented by one just after
		/// each call to loop
		u64 tick;
	};

	void startEngine(const EngineSettings& settings){
		Context cntx = {0};

		xen::Stopwatch timer;
		bool loop_return_code;
		xen::Duration last_time = timer.getElapsedTime();

		printf("Engine init finished, beginning main loop...\n");
		do {
			cntx.time = timer.getElapsedTime();
			cntx.dt = cntx.time - last_time;

			loop_return_code = settings.loop(cntx);

			++cntx.tick;
			last_time = cntx.time;
		} while (loop_return_code);

		printf("Main loop requested termination, doing engine cleanup\n");
		// free resources, check for memory leaks, etc
		printf("Exiting\n");
	}
}

bool mainLoop(const xen::Context& cntx);
int main(){
	printf("Hello world!\n");

	xen::EngineSettings settings;
	settings.loop = &mainLoop;

	xen::startEngine(settings);

	printf("End of main\n");
}

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
