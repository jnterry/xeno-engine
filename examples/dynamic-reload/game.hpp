#include <xen/kernel/Kernel.hpp>
#include <xen/kernel/Module.hpp>

struct GameApi {
	xen::TickFunction tick;
};
