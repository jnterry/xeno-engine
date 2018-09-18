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
#include "Shader.hxx"
#include "Texture.hxx"
#include "RenderTarget.hxx"
#include "render.hxx"

#include <cstring>

void doRasterizerStateInit(void* block, const u64 BLK_SIZE);

class RasterizerDevice : public xen::GraphicsDevice {
public:
	~RasterizerDevice(){}

	RasterizerDevice(xen::Array<xsr::PostProcessor*> post_processors)
	{
		void* data = malloc(xen::megabytes(4));
	  doRasterizerStateInit(data, xen::megabytes(4));
	}

	xsr::RenderTarget* getRenderTargetImpl(xen::RenderTarget target){
		return xsr::getRenderTargetImpl(target);
	}

	void clear(xen::RenderTarget& target, xen::Color color){
		return xsr::clear(target, color);
	}

	xen::RenderTarget createRenderTarget (Vec2u size, xen::Window* window){
		return xsr::createRenderTarget(size, window);
	}

	void destroyRenderTarget(xen::RenderTarget render_target){
		return xsr::destroyRenderTarget(render_target);
	}

	void resizeRenderTarget(xsr::RenderTarget* target, Vec2u size){
		return xsr::resizeRenderTarget(target, size);
	}

	xen::Window* createWindow(Vec2u size, const char* title) {
		return xsr::createWindow(size, title);
	}

	void destroyWindow(xen::Window* window) {
		return xsr::destroyWindow(window);
	}

	void swapBuffers(xen::Window* window) {
		return xsr::swapBuffers(window);
	}

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




	xen::Shader createShader(const void* source){
		return xsr::createShader(source);
	}
	void destroyShader(xen::Shader shader){
		return xsr::destroyShader(shader);
	}



	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {

		xsr::render(target_handle, viewport, params, commands);
	}
};

namespace xen {
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<xsr::PostProcessor*> post_processors){
		return xen::emplace<RasterizerDevice>(arena, post_processors);
	}
}

#endif
