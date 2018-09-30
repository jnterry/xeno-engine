////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of exported rasterizer kernel module
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP
#define XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP

#include "ModuleCommon.cpp"

namespace {
	void tick(const xen::TickContext& tick){
		for(u32 i = 0; i < xsr::state->next_free_op; ++i){
			xen::RenderOp& op = xsr::state->op_list[i];

			switch(op.type){
			case xen::RenderOp::CLEAR:
				xsr::clear(op.clear.target, op.clear.color);
				break;
			case xen::RenderOp::DRAW:
				xsr::render(op.draw.target, op.draw.viewport, *op.draw.params, op.draw.commands);
				break;
			case xen::RenderOp::SWAP_BUFFERS:
				xsr::swapBuffers(op.swap_buffers.window);
				break;
			}
		}
		xsr::state->next_free_op = 0;
	}

}

XenDeclareModule("graphics", &init, &shutdown, &load, nullptr, &tick);

#endif
