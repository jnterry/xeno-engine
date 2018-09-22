////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of exported rasterizer kernel module
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP
#define XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP

#include "ModuleRasterize.hxx"
#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/sren/rasterizer3d.hxx>
#include <xen/kernel/Kernel.hpp>
#include <xen/core/memory/utilities.hpp>

#include "Shader.hxx"
#include "Texture.hxx"
#include "RenderTarget.hxx"
#include "Mesh.hxx"
#include "render.hxx"

#include <new>

namespace xen {
	struct Kernel;
}

xsr::ModuleRasterize* xsr::state = nullptr;

void doRasterizerStateInit(void* block, const u64 BLK_SIZE){
	xsr::state = (xsr::ModuleRasterize*)block;

	xsr::state = (xsr::ModuleRasterize*)block;
	xen::clearToZero(xsr::state);

	xsr::state->root_arena.start     = xen::ptrGetAdvanced(block, sizeof(xsr::ModuleRasterize));
	xsr::state->root_arena.end       = xen::ptrGetAdvanced(block, BLK_SIZE);
	xsr::state->root_arena.next_byte = xsr::state->root_arena.start;

	xsr::state->mesh_pool            = xen::createArenaPool<xsr::RasterizerMesh >(xsr::state->root_arena, 128);
	xsr::state->mesh_attrib_alloc    = xen::emplace<xen::AllocatorMalloc>(xsr::state->root_arena);

	xsr::state->texture_pool         = xen::createArenaPool<xsr::Texture>(xsr::state->root_arena, 128);
	xsr::state->texture_pixel_alloc  = xen::emplace<xen::AllocatorMalloc>(xsr::state->root_arena);

	xsr::state->shader_pool          = xen::createArenaPool<xsr::FragmentShader>(xsr::state->root_arena, 128);

	xsr::state->render_target_pool   = xen::createArenaPool<xsr::RenderTarget>(xsr::state->root_arena, 128);
	xsr::state->render_target_alloc  = xen::emplace<xen::AllocatorMalloc>(xsr::state->root_arena);

	xsr::state->thpool = thpool_init(16);

	// Ensure texture 0 is single pixel white
	xen::RawImage image;
	image.size.x = 1;
	image.size.y = 1;
	xen::Color color = xen::Color::WHITE;
	image.pixels = &color;
	xsr::createTexture(&image);

	// Ensure shader 0 is the default shader
	xsr::createShader((void*)xsr::FragmentShader_Default);
}

namespace {
	void* init(xen::Kernel& kernel, const void* params){
		const u64 BLK_SIZE = xen::megabytes(4);

		void* data = xen::allocate(kernel, BLK_SIZE, alignof(xsr::ModuleRasterize));
		if(data == nullptr){ return nullptr; }

		doRasterizerStateInit(data, BLK_SIZE);

		return data;
	}

	void shutdown(xen::Kernel& kernel){
		// :TODO: deallocate everything else, eg, mesh data
		xen::deallocate(kernel, xsr::state);
	}

	void* load(xen::Kernel& kernel, void* data, const void* params){
		xsr::state = (xsr::ModuleRasterize*)data;

		xen::GraphicsModuleApi& api = xsr::state->api;


		api.createWindow            = &xsr::createWindow;
		api.destroyWindow           = &xsr::destroyWindow;
		api.swapBuffers             = &xsr::swapBuffers;
		api._createMeshFromMeshData = &xsr::createMesh;
		api.destroyMesh             = &xsr::destroyMesh;
		api._updateMeshVertexData   = &xsr::updateMeshVertexData;
		api.createTexture           = &xsr::createTexture;
		api.destroyTexture          = &xsr::destroyTexture;
		api.createShader            = &xsr::createShader;
		api.destroyShader           = &xsr::destroyShader;
		api._clearTarget            = &xsr::clear;
		api._renderToTarget         = &xsr::render;

		return &api;
	}

	void tick(xen::Kernel& kernel, const xen::TickContext& tick){
		// no-op
	}
}

xen::Module exported_xen_module = {
	xen::hash("graphics"),
	&init,
	&shutdown,
	&load,
	&tick
};

#endif
