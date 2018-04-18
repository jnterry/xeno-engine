////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of a base class for software device renderers
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICEBASE_CPP
#define XEN_SREN_SOFTWAREDEVICEBASE_CPP

#include "SoftwareDeviceBase.hxx"
#include "render-utilities.hxx"
#include "RenderTargetImpl.hxx"

#include "../graphics/Window.hxx"

#include <xen/sren/PostProcessor.hpp>
#include <xen/core/array.hpp>

namespace xen {
	namespace sren {

		SoftwareDeviceBase::SoftwareDeviceBase(xen::Array<PostProcessor*> post_processors)
			: post_processors(post_processors),
			  main_allocator (new xen::AllocatorCounter<xen::AllocatorMalloc>()),
			  misc_arena     (xen::createArenaLinear(*main_allocator, xen::megabytes(1))),
			  render_targets (xen::createArenaPool<RenderTargetImpl>(main_allocator, 128))
		{
			this->thpool = thpool_init(4);
		}

		SoftwareDeviceBase::~SoftwareDeviceBase(){
			xen::destroyArenaLinear(*main_allocator, misc_arena);
			delete main_allocator;
			thpool_destroy(this->thpool);
		}

		RenderTargetImpl* SoftwareDeviceBase::getRenderTargetImpl(RenderTarget target){
			return &this->render_targets.slots[target._id].item;
		}

		void SoftwareDeviceBase::clear(xen::RenderTarget& target, xen::Color color){
			xen::sren::clear(*this->getRenderTargetImpl(target), color);
		}

		RenderTarget SoftwareDeviceBase::createRenderTarget (Vec2u size, Window* window){
			// :TODO:COMP::ISSUE_31: object pool with automatic handles / resizeable pool
			u32 slot = xen::reserveSlot(this->render_targets);
			RenderTargetImpl* target = &this->render_targets.slots[slot].item;

			xen::clearToZero<RenderTargetImpl>(target);
			this->resizeRenderTarget(target, size);

			target->window = window;
			xen::sren::doPlatformRenderTargetInit(this->main_allocator, *target, target->window);

			return this->makeHandle<RenderTarget::HANDLE_ID>(slot, 0);
		}

		void SoftwareDeviceBase::destroyRenderTarget(RenderTarget render_target){
			RenderTargetImpl* target = getRenderTargetImpl(render_target);

			this->main_allocator->deallocate(target->color);
			this->main_allocator->deallocate(target->depth);

			target->color = nullptr;
			target->depth = nullptr;

			xen::sren::doPlatformRenderTargetDestruction(this->main_allocator, *target, target->window);

			xen::freeType(this->render_targets, target);
		}

		void SoftwareDeviceBase::resizeRenderTarget(RenderTargetImpl* target, Vec2u size){
			target->size = size;

			if(target->color != nullptr){
				main_allocator->deallocate(target->color);
			}
			if(target->depth != nullptr){
				main_allocator->deallocate(target->depth);
			}

			u32 num_pixels = size.x * size.y;

			target->color = (Color4f*)main_allocator->allocate(sizeof(Color4f) * num_pixels);
			target->depth = (float*  )main_allocator->allocate(sizeof(float  ) * num_pixels);

			xen::sren::doPlatformRenderTargetResize(main_allocator, *target, target->window);
		}

		Window* SoftwareDeviceBase::createWindow(Vec2u size, const char* title) {
		  xen::MemoryTransaction transaction(misc_arena);

			xen::Window* window = xen::impl::createWindow(misc_arena, size, title);

			window->render_target    = this->createRenderTarget(size, window);
			RenderTargetImpl* target = this->getRenderTargetImpl(window->render_target);

			target->window = window;

			transaction.commit();
			return window;
		}

		void SoftwareDeviceBase::destroyWindow(Window* window) {
			destroyRenderTarget(window->render_target);
			xen::impl::destroyWindow(window);
			window->is_open = false;
		}

		void SoftwareDeviceBase::swapBuffers(Window* window) {
			if(!window->is_open){ return; }
			RenderTargetImpl& target = *this->getRenderTargetImpl(window->render_target);

			for(u32 i = 0; i < xen::size(this->post_processors); ++i){
				this->post_processors[i]->process(target);
			}

			xen::sren::presentRenderTarget(window, target);
		}
	}
}

#endif
