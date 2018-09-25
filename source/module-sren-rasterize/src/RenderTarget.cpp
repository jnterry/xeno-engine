////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of shader management functions
///
/// \ingroup module-sren-texture
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_RENDERTARGET_CPP
#define XEN_MODULESRENRASTERIZE_RENDERTARGET_CPP

#include "RenderTarget.hxx"
#include "ModuleRasterize.hxx"
#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/Window.hxx>
#include <xen/sren/RenderTarget.hxx>
#include <xen/sren/PostProcessor.hpp>
#include <xen/core/memory/ArenaLinear.hpp>


xsr::RenderTarget* xsr::getRenderTargetImpl(xen::RenderTarget target){
	return &xsr::state->render_target_pool.slots[target._id].item;
}

void xsr::clear(xen::RenderTarget target, xen::Color color){
	xsr::clear(*xsr::getRenderTargetImpl(target), color);
}

xen::RenderTarget xsr::createRenderTarget (Vec2u size, xen::Window* window){
	// :TODO:COMP::ISSUE_31: object pool with automatic handles / resizeable pool
	u32 slot = xen::reserveSlot(xsr::state->render_target_pool);
	xsr::RenderTarget* target = &xsr::state->render_target_pool.slots[slot].item;

	xen::clearToZero<xsr::RenderTarget>(target);
	xsr::resizeRenderTarget(target, size);

	target->window = window;
	xsr::doPlatformRenderTargetInit(xsr::state->render_target_alloc, *target, target->window);

	return xen::makeGraphicsHandle<xen::RenderTarget::HANDLE_ID>(slot, 0);
}

void xsr::destroyRenderTarget(xen::RenderTarget render_target){
	xsr::RenderTarget* target = getRenderTargetImpl(render_target);

	xsr::state->render_target_alloc->deallocate(target->color);
	xsr::state->render_target_alloc->deallocate(target->depth);

	target->color = nullptr;
	target->depth = nullptr;

	xsr::doPlatformRenderTargetDestruction(xsr::state->render_target_alloc, *target, target->window);

	xen::freeType(xsr::state->render_target_pool, target);
}

void xsr::resizeRenderTarget(xsr::RenderTarget* target, Vec2u size){
	target->size = size;

	xen::Allocator* alloc = xsr::state->render_target_alloc;

	if(target->color != nullptr){
	  alloc->deallocate(target->color);
	}
	if(target->depth != nullptr){
		alloc->deallocate(target->depth);
	}

	u32 num_pixels = size.x * size.y;

	target->color = (xen::Color4f*)alloc->allocate(sizeof(xen::Color4f) * num_pixels);
	target->depth = (float*       )alloc->allocate(sizeof(float       ) * num_pixels);

	xsr::doPlatformRenderTargetResize(alloc, *target, target->window);
}

xen::Window* xsr::createWindow(Vec2u size, const char* title) {
	xen::MemoryTransaction transaction(xsr::state->root_arena);

	xen::Window* window = xen::impl::createWindow(xsr::state->root_arena, size, title);

	window->render_target     = xsr::createRenderTarget(size, window);
	xsr::RenderTarget* target = xsr::getRenderTargetImpl(window->render_target);

	target->window = window;

	transaction.commit();
	return window;
}

void xsr::destroyWindow(xen::Window* window) {
	destroyRenderTarget(window->render_target);
	xen::impl::destroyWindow(window);
	window->is_open = false;
}

void xsr::swapBuffers(xen::Kernel& kernel, xen::Window* window) {
	if(!window->is_open){ return; }
	xsr::RenderTarget& target = *xsr::getRenderTargetImpl(window->render_target);

	for(u32 i = 0; i < xsr::state->post_processors.size; ++i){
		if(xsr::state->post_processors[i]->disabled){ continue; }

		xsr::state->post_processors[i]->process(target);
	}

	xsr::presentRenderTarget(kernel, window, target);
}


#endif
