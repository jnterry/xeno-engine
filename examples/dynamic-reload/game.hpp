#include <xen/kernel/Kernel.hpp>
#include <xen/kernel/Module.hpp>

struct Game : public xen::Module {
	Game();

	xen::TickFunction tick;
};
