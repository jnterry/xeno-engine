////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the OpenGL Graphics Device
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_GLDEVICE_CPP
#define XEN_GL_GLDEVICE_CPP

#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/array.hpp>
#include <xen/math/utilities.hpp>
#include <xen/config.hpp>

#include "gl_header.hxx"
#include "Mesh.hxx"
#include "Shader.hxx"
#include "Texture.hxx"
#include "RenderTarget.hxx"
#include "render.hxx"
#include <xen/graphics/Window.hxx>


#include <GL/glew.h>

#include "ModuleGl.hxx"
#include "Window.hxx"

void doGlStateInit(void* memory_block, const u64 BLK_SIZE);

namespace {

	class GlDevice : public xen::GraphicsDevice {
	public:
		~GlDevice(){}

		GlDevice() {
			void* block = malloc(xen::megabytes(1));
			doGlStateInit(block, xen::megabytes(1));
		}

		xen::Window* createWindow(Vec2u size, const char* title){
			return xgl::createWindow(size, title);
		}
		void destroyWindow(xen::Window* window){
			return xgl::destroyWindow(window);
		}
		void swapBuffers(xen::Window* window){
			return xgl::swapBuffers(window);
		}

		xen::Mesh createMesh(const xen::MeshData* mesh_data){
			return xgl::createMesh(mesh_data);
		}

		void updateMeshVertexData(xen::Mesh mesh_handle,
		                          u32 attrib_index,
		                          void* new_data,
		                          u32 start_vertex,
		                          u32 end_vertex
		                          ) {
			xgl::updateMeshVertexData(mesh_handle, attrib_index, new_data, start_vertex, end_vertex);
		}

		void destroyMesh(xen::Mesh mesh) {
			xgl::destroyMesh(mesh);
		}

		xen::Texture createTexture(const xen::RawImage* image) override {
			return xgl::createTexture(image);
		}

		void destroyTexture(xen::Texture texture) override {
			return xgl::destroyTexture(texture);
		}

		xen::Shader createShader(const void* source) override {
			return xen::makeNullGraphicsHandle<xen::Shader>();
			// :TODO: implement
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys an existing Shader
		/////////////////////////////////////////////////////////////////////
		void destroyShader(xen::Shader texture) override {
			// :TODO: implement
		}

		void clear(xen::RenderTarget& render_target, xen::Color color) override {
			xgl::clearTarget(render_target, color);
		}

		void render(xen::RenderTarget render_target,
		            const xen::Aabb2u& viewport,
		            const xen::RenderParameters3d& params,
		            const xen::Array<xen::RenderCommand3d> commands
		            ) {
			xgl::render(render_target, viewport, params, commands);
		}
		/// @}
	};
}

namespace xen {
	GraphicsDevice* createGlDevice(ArenaLinear& arena){
		return xen::emplace<GlDevice>(arena);
	}
}

#endif
