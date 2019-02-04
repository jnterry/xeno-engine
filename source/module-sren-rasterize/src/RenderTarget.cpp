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
#include <xen/window/Window.hxx>
#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/sren/RenderTarget.hxx>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/bits.hpp>


xsr::RenderTarget* xsr::getRenderTargetImpl(xen::RenderTarget target){
	return &xsr::state->render_target_pool.slots[target._id].item;
}

void xsr::clear(xen::RenderTarget target, xen::Color color){
	xsr::clear(*xsr::getRenderTargetImpl(target), color);
}

xen::RenderTarget xsr::createWindowRenderTarget(xen::Window* window) {
	xen::MemoryTransaction transaction(xsr::state->root_arena);

	u32 slot = xen::reserveSlot(xsr::state->render_target_pool);
	xsr::RenderTarget* target = &xsr::state->render_target_pool.slots[slot].item;
	xen::RenderTarget handle = xen::makeGraphicsHandle<xen::RenderTarget::HANDLE_ID>(slot, 0);

	xen::clearToZero<xsr::RenderTarget>(target);
	xsr::resizeRenderTarget(target, window->size);
	target->window = window;

	xsr::doPlatformRenderTargetInit(xsr::state->render_target_alloc, *target, window);

	transaction.commit();
	return handle;
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

void xsr::swapBuffers(xen::RenderTarget handle) {
	xsr::RenderTarget& target = *xsr::getRenderTargetImpl(handle);
	xen::Window* window = target.window;

	if(window == nullptr || !(window->state & xen::Window::IS_OPEN)){ return; }

	xsr::presentRenderTarget(window, target);
}


#endif
