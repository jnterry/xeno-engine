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
#include <xen/sren/render-debug.hxx>
#include <xen/sren/RenderTarget.hxx>

#include <xen/graphics/Window.hxx>

#include <xen/sren/PostProcessor.hpp>
#include <xen/core/array.hpp>
#include <xen/graphics/Image.hpp>

#include <cstring>

namespace xen {
	namespace sren {

		SoftwareDeviceBase::SoftwareDeviceBase(xen::Array<xsren::PostProcessor*> post_processors)
			: post_processors(post_processors),
			  main_allocator (new xen::AllocatorCounter<xen::AllocatorMalloc>()),
			  misc_arena     (xen::createArenaLinear(*main_allocator, xen::megabytes(1))),
			  render_targets (xen::createArenaPool<xsren::RenderTarget  >(main_allocator,  128)),
			  textures       (xen::createArenaPool<xsren::Texture       >(main_allocator, 1024)),
			  shaders        (xen::createArenaPool<xsren::FragmentShader>(main_allocator,  128))
		{
			this->thpool = thpool_init(4);

			// Ensure shader 0 is the default shader
			this->createShader((void*)xsren::FragmentShader_Default);

			// Ensure texture 0 is single pixel white
			RawImage image;
			image.size.x = 1;
			image.size.y = 1;
			xen::Color color = xen::Color::WHITE;
			image.pixels = &color;
			this->createTexture(&image);
		}

		SoftwareDeviceBase::~SoftwareDeviceBase(){
			xen::destroyArenaLinear(*main_allocator, misc_arena);
			delete main_allocator;
			thpool_destroy(this->thpool);
		}

		xsren::RenderTarget* SoftwareDeviceBase::getRenderTargetImpl(RenderTarget target){
			return &this->render_targets.slots[target._id].item;
		}

		xsren::Texture* SoftwareDeviceBase::getTextureImpl(Texture texture){
			return &this->textures.slots[texture._id].item;
		}

		xsren::FragmentShader SoftwareDeviceBase::getShaderImpl(Shader shader){
			return this->shaders.slots[shader._id].item;
		}

		Texture SoftwareDeviceBase::createTexture(const RawImage* image){
			u32 slot = xen::reserveSlot(textures);

			Texture result = xen::makeGraphicsHandle<Texture::HANDLE_ID>(slot, 0);

			xsren::Texture* timpl = this->getTextureImpl(result);

			u32 num_bytes = sizeof(xen::Color) * image->width * image->height;

			timpl->image.size = image->size;
			timpl->image.pixels = (xen::Color*)main_allocator->allocate(num_bytes);
			memcpy(timpl->image.pixels, image->pixels, num_bytes);

			return result;
		}

		void SoftwareDeviceBase::destroyTexture(Texture texture){
			xsren::Texture* timpl = this->getTextureImpl(texture);
			main_allocator->deallocate(timpl->image.pixels);
			xen::freeSlot(textures, texture._id);
		}

		Shader SoftwareDeviceBase::createShader(const void* source){
			u32 slot = xen::reserveSlot(shaders);
			shaders.slots[slot].item = (xsren::FragmentShader)source;
			return makeGraphicsHandle<Shader::HANDLE_ID>(slot, 0);
		}

		void SoftwareDeviceBase::destroyShader(Shader shader){
			xen::freeSlot(shaders, shader._id);
		}

		void SoftwareDeviceBase::clear(xen::RenderTarget& target, xen::Color color){
			xsren::clear(*this->getRenderTargetImpl(target), color);
		}

		RenderTarget SoftwareDeviceBase::createRenderTarget (Vec2u size, Window* window){
			// :TODO:COMP::ISSUE_31: object pool with automatic handles / resizeable pool
			u32 slot = xen::reserveSlot(this->render_targets);
			xsren::RenderTarget* target = &this->render_targets.slots[slot].item;

			xen::clearToZero<xsren::RenderTarget>(target);
			this->resizeRenderTarget(target, size);

			target->window = window;
			xsren::doPlatformRenderTargetInit(this->main_allocator, *target, target->window);

			return xen::makeGraphicsHandle<RenderTarget::HANDLE_ID>(slot, 0);
		}

		void SoftwareDeviceBase::destroyRenderTarget(RenderTarget render_target){
			xsren::RenderTarget* target = getRenderTargetImpl(render_target);

			this->main_allocator->deallocate(target->color);
			this->main_allocator->deallocate(target->depth);

			target->color = nullptr;
			target->depth = nullptr;

			xsren::doPlatformRenderTargetDestruction(this->main_allocator, *target, target->window);

			xen::freeType(this->render_targets, target);
		}

		void SoftwareDeviceBase::resizeRenderTarget(xsren::RenderTarget* target, Vec2u size){
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

			xsren::doPlatformRenderTargetResize(main_allocator, *target, target->window);
		}

		Window* SoftwareDeviceBase::createWindow(Vec2u size, const char* title) {
		  xen::MemoryTransaction transaction(misc_arena);

			xen::Window* window = xen::impl::createWindow(misc_arena, size, title);

			window->render_target    = this->createRenderTarget(size, window);
			xsren::RenderTarget* target = this->getRenderTargetImpl(window->render_target);

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
			xsren::RenderTarget& target = *this->getRenderTargetImpl(window->render_target);

			for(u32 i = 0; i < xen::size(this->post_processors); ++i){
				if(this->post_processors[i]->disabled){ continue; }

				this->post_processors[i]->process(target);
			}

		  xsren::presentRenderTarget(window, target, thpool);
		}
	}
}

#endif
