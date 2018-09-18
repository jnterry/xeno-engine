////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Rasterizer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZERDEVICE_CPP
#define XEN_SREN_RASTERIZERDEVICE_CPP

#include <xen/sren/RasterizerDevice.hpp>
#include <xen/sren/FragmentShader.hpp>
#include <xen/sren/PostProcessor.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/graphics/Window.hxx>
#include <xen/math/geometry.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/intrinsics.hpp>

#include <xen/sren/SoftwareDeviceBase.hxx>
#include <xen/sren/render-debug.hxx>
#include <xen/sren/rasterizer3d.hxx>
#include <xen/sren/MeshStore.hxx>

#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/Allocator.hpp>

#include <xen/sren/FragmentShader.hpp>
#include <xen/sren/Texture.hpp>
#include <xen/sren/RenderTarget.hxx>

#include <thpool.h>

#include "ModuleRasterize.hxx"
#include "Mesh.hxx"

#include <cstring>

#include <thpool.h>

void doRasterizerStateInit(void* block, const u64 BLK_SIZE);

class RasterizerDevice : public xen::GraphicsDevice {
private:
	xen::Array<xsr::PostProcessor*> post_processors;
protected:
	xen::Allocator*  main_allocator;
	xen::ArenaLinear misc_arena;

	xen::ArenaPool<xsr::RenderTarget  > render_targets;
	xen::ArenaPool<xsr::FragmentShader> shaders;

	threadpool thpool;

public:
	~RasterizerDevice(){
		xen::destroyArenaLinear(*main_allocator, misc_arena);
		delete main_allocator;
		thpool_destroy(this->thpool);
	}

	RasterizerDevice(xen::Array<xsr::PostProcessor*> post_processors)
	  : post_processors(post_processors),
	    main_allocator (new xen::AllocatorCounter<xen::AllocatorMalloc>()),
	    misc_arena     (xen::createArenaLinear(*main_allocator, xen::megabytes(1))),
	    render_targets (xen::createArenaPool<xsr::RenderTarget  >(main_allocator,  128)),
	    shaders        (xen::createArenaPool<xsr::FragmentShader>(main_allocator,  128))
	{
		this->thpool = thpool_init(4);

		void* data = malloc(xen::megabytes(4));
	  doRasterizerStateInit(data, xen::megabytes(4));

	  // Ensure shader 0 is the default shader
	  this->createShader((void*)xsr::FragmentShader_Default);
	}

	xsr::RenderTarget* getRenderTargetImpl(xen::RenderTarget target){
		return &this->render_targets.slots[target._id].item;
	}

	xsr::FragmentShader getShaderImpl(xen::Shader shader){
		return this->shaders.slots[shader._id].item;
	}

	xen::Shader createShader(const void* source){
		u32 slot = xen::reserveSlot(shaders);
		shaders.slots[slot].item = (xsr::FragmentShader)source;
		return xen::makeGraphicsHandle<xen::Shader::HANDLE_ID>(slot, 0);
	}

	void destroyShader(xen::Shader shader){
		xen::freeSlot(shaders, shader._id);
	}

	void clear(xen::RenderTarget& target, xen::Color color){
		xsr::clear(*this->getRenderTargetImpl(target), color);
	}

	xen::RenderTarget createRenderTarget (Vec2u size, xen::Window* window){
		// :TODO:COMP::ISSUE_31: object pool with automatic handles / resizeable pool
		u32 slot = xen::reserveSlot(this->render_targets);
		xsr::RenderTarget* target = &this->render_targets.slots[slot].item;

		xen::clearToZero<xsr::RenderTarget>(target);
		this->resizeRenderTarget(target, size);

		target->window = window;
		xsr::doPlatformRenderTargetInit(this->main_allocator, *target, target->window);

		return xen::makeGraphicsHandle<xen::RenderTarget::HANDLE_ID>(slot, 0);
	}

	void destroyRenderTarget(xen::RenderTarget render_target){
		xsr::RenderTarget* target = getRenderTargetImpl(render_target);

		this->main_allocator->deallocate(target->color);
		this->main_allocator->deallocate(target->depth);

		target->color = nullptr;
		target->depth = nullptr;

		xsr::doPlatformRenderTargetDestruction(this->main_allocator, *target, target->window);

		xen::freeType(this->render_targets, target);
	}

	void resizeRenderTarget(xsr::RenderTarget* target, Vec2u size){
		target->size = size;

		if(target->color != nullptr){
			main_allocator->deallocate(target->color);
		}
		if(target->depth != nullptr){
			main_allocator->deallocate(target->depth);
		}

		u32 num_pixels = size.x * size.y;

		target->color = (xen::Color4f*)main_allocator->allocate(sizeof(xen::Color4f) * num_pixels);
		target->depth = (float*  )main_allocator->allocate(sizeof(float  ) * num_pixels);

		xsr::doPlatformRenderTargetResize(main_allocator, *target, target->window);
	}

	xen::Window* createWindow(Vec2u size, const char* title) {
		xen::MemoryTransaction transaction(misc_arena);

		xen::Window* window = xen::impl::createWindow(misc_arena, size, title);

		window->render_target    = this->createRenderTarget(size, window);
		xsr::RenderTarget* target = this->getRenderTargetImpl(window->render_target);

		target->window = window;

		transaction.commit();
		return window;
	}

	void destroyWindow(xen::Window* window) {
		destroyRenderTarget(window->render_target);
		xen::impl::destroyWindow(window);
		window->is_open = false;
	}

	void swapBuffers(xen::Window* window) {
		if(!window->is_open){ return; }
		xsr::RenderTarget& target = *this->getRenderTargetImpl(window->render_target);

		for(u32 i = 0; i < xen::size(this->post_processors); ++i){
			if(this->post_processors[i]->disabled){ continue; }

			this->post_processors[i]->process(target);
		}

		xsr::presentRenderTarget(window, target, thpool);
	}


	//
	//
	//
	//
	//
	//
	//
	//
	//
	//
	//
	//
	//
	//
	//
	//

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override{
		return xsr::createMesh(mesh_data);
	}
	void      destroyMesh         (xen::Mesh mesh) override{
		return xsr::destroyMesh(mesh);
	}
	void      updateMeshVertexData(xen::Mesh mesh,
	                               u32   attrib_index,
	                               void* new_data,
	                               u32   start_vertex,
	                               u32   end_vertex) override{
		return xsr::updateMeshVertexData(mesh,
		                                 attrib_index,
		                                 new_data,
		                                 start_vertex,
		                                 end_vertex
		                                );
	}

	xen::Texture createTexture(const xen::RawImage* image){
		return xsr::createTexture(image);
	}

	void destroyTexture(xen::Texture texture){
		xsr::destroyTexture(texture);
	}

	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xsr::RenderTarget& target = *this->getRenderTargetImpl(target_handle);

		////////////////////////////////////////////////////////////////////////////
		// Generate view projection matrix
		if(!xen::isCameraValid(params.camera)){
			printf("ERROR: Camera is not valid, skipping rendering\n");
			return;
		}

		// Find the actual view_region we wish to draw to. This is the
		// intersection of the actual target, and the user specified viewport
		xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
		xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

		Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

		if(xen::isnan(vp_matrix)){
			// :TODO: log
			printf("ERROR: vp_matrix contains NaN elements, skipping rendering\n");
			return;
		}
		////////////////////////////////////////////////////////////////////////////

		xsr::RasterizationContext context;
		context.target          = &target;
		context.viewport        = &view_region;

		*(xen::RenderParameters3d*)(&context) = params;
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			const xen::RenderCommand3d& cmd = commands[cmd_index];

			context.fragment_shader = this->getShaderImpl(cmd.shader);
			if(context.fragment_shader == nullptr){
				context.fragment_shader = xsr::FragmentShader_Default;
			}
			setPerCommandFragmentUniforms(context,
			                              (xen::Material&)cmd,
			                              cmd.model_matrix,
			                              vp_matrix
			                             );
			context.textures[0] = xsr::getTextureImpl(cmd.textures[0]);
			context.textures[1] = xsr::getTextureImpl(cmd.textures[1]);
			context.textures[2] = xsr::getTextureImpl(cmd.textures[2]);
			context.textures[3] = xsr::getTextureImpl(cmd.textures[3]);
			auto mesh = xsr::getMeshImpl(cmd.mesh);

			#if 0
			renderDebugBoundingBox(context,
			                       xen::getTransformed(mesh->bounds, commands[cmd_index].model_matrix)
			                      );
			#endif

			rasterizeMesh(context, cmd.primitive_type, *mesh);
		}
	}
};

namespace xen {
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<xsr::PostProcessor*> post_processors){
		return xen::emplace<RasterizerDevice>(arena, post_processors);
	}
}

#endif
