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
#include "renderer3d.hxx"
#include "RenderTargetImpl.hxx"

#include "../graphics/Window.hxx"

#include <xen/core/array.hpp>

namespace xen {
	namespace sren {

		SoftwareDeviceBase::SoftwareDeviceBase()
			: main_allocator(new xen::AllocatorCounter<xen::AllocatorMalloc>()),
			  misc_arena       (xen::createArenaLinear(*main_allocator, xen::megabytes(1))) {

		}

		SoftwareDeviceBase::~SoftwareDeviceBase(){
			xen::destroyArenaLinear(*main_allocator, misc_arena);
			delete main_allocator;
		}

		void SoftwareDeviceBase::clear(xen::RenderTarget target, const xen::Aabb2u& viewport, xen::Color color){
			xen::sren::clear(*this->getRenderTargetImpl(target), viewport, color);
		}

		RenderTarget SoftwareDeviceBase::createRenderTarget (Vec2u size, Window* window){
			u32 slot;
			for(slot = 0; slot < xen::size(this->render_targets); ++slot){
				if(this->render_targets[slot].elements == nullptr){
					break;
				}
			}
			// :TODO: re-sizeable pool
			XenAssert(slot <= xen::size(this->render_targets), "No space for new render target");

			RenderTargetImpl* target = &this->render_targets[slot];

			xen::clearToZero<RenderTargetImpl>(target);
			this->resizeRenderTarget(target, size);

			xen::sren::doPlatformRenderTargetInitialization(target, window);

			return this->makeHandle<RenderTarget::HANDLE_ID>(slot, 0);
		}

		void SoftwareDeviceBase::destroyRenderTarget(RenderTarget render_target){
			RenderTargetImpl* target = getRenderTargetImpl(render_target);

			this->main_allocator->deallocate(target->elements);
			target->elements = nullptr;
		}

		RenderTargetImpl* SoftwareDeviceBase::getRenderTargetImpl(RenderTarget target){
			return &this->render_targets[target._id];
		}

		void SoftwareDeviceBase::resizeRenderTarget(RenderTargetImpl* target, Vec2u size){
			RenderTargetPixel* old_pixels = target->elements;

			target->elements =
				(RenderTargetPixel*)main_allocator->allocate(sizeof(RenderTargetPixel) * size.x * size.y);
			// Note: while x is really a column rather than row, access is [r][c],
			// and we want to access as [x][y], hence swapping here
			target->rows = size.x;
			target->cols = size.y;

			if(old_pixels){
				main_allocator->deallocate(target->elements);
			}
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
			RenderTargetImpl* target = this->getRenderTargetImpl(window->render_target);
		}
	}
}

#endif
