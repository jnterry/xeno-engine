////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Creates and exports the xen::Module instance
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_MODULE_CPP
#define XEN_GL_MODULE_CPP

#include <xen/graphics/GraphicsModuleApi.hpp>
#include <xen/kernel/Module.hpp>
#include <xen/kernel/Kernel.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/String.hpp>

#include "ModuleGl.hxx"
#include "Window.hxx"
#include "Texture.hxx"
#include "Mesh.hxx"
#include "RenderTarget.hxx"
#include "render.hxx"

		/*void xenGlDebugCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* msg, onst void* userParam){
		  const char* source_str = "Unknown Source";
		  const char* type_str   = "Unknown Type";

		  switch(source){
		  case DEBUG_SOURCE_API:
		  source_str = "DEBUG_SOURCE_API (call to gl function)"; break;
		  case DEBUG_SOURCE_WINDOW_SYSTEM:
		  source_str = "DEBUG_SOURCE_WINDOW_SYSTEM"; break;
		  case DEBUG_SOURCE_SHADER_COMPILER:
		  source_str = "DEBUG_SOURCE_SHADER_COMPILER"; break;
		  case DEBUG_SOURCE_THIRD_PARTY:
		  source_str = "DEBUG_SOURCE_THRID_PARTY"; break;
		  case DEBUG_SOURCE_APPLICATION:
		  source_str = "DEBUG_SOURCE_APPLICATION"; break;
		  case DEBUG_SOURCE_OTHER:
		  source_str = "DEBUG_SOURCE_OTHER";break;
		  default: XenInvalidPath;
		  };

		  switch(type){
		  case DEBUG_TYPE_ERROR:               type_str = "DEBUG_TYPE_ERROR"; break;
		  case DEBUG_TYPE_DEPRECATED_BEHAVIOR: type_str = "DEBUG_TYPE_DEPRECATED_BEHAVIOR"; break;
		  case DEBUG_TYPE_UNDEFINED_BEHAVIOR:  type_str = "DEBUG_TYPE_UNDEFINED_BEHAVIOR"; break;
		  case DEBUG_TYPE_PORTABILITY:         type_str = "DEBUG_TYPE_PORTABILITY"; break;
		  case DEBUG_TYPE_PERFORMANCE:         type_str = "DEBUG_TYPE_PERFORMANCE"; break;
		  case DEBUG_TYPE_MARKER:              type_str = "DEBUG_TYPE_MARKER"; break;
		  case DEBUG_TYPE_PUSH_GROUP:          type_str = "DEBUG_TYPE_PUSH_GROUP"; break;
		  case DEBUG_TYPE_POP_GROUP:           type_str = "DEBUG_TYPE_POP_GROUP"; break;
		  case DEBUG_TYPE_OTHER:               type_str = "DEBUG_TYPE_OTHER"; break;
		  default: XenInvalidPath;
		  };

		  xen::log::write(::xen::_sys::log, "?", 0, xen::log::ERROR, "OpenGL Error Callback: source: '%s', type: '%s', msg: %s",
		  source_str, type_str, msg, "xen.gl", nullptr);

		  }*/


xgl::GlState* xgl::gl_state = nullptr;

// :TODO: this function temporary so we can call from GlDevice... once we
// remove GraphicsDevice code this can be folded into init
void doGlStateInit(void* memory_block, const u64 BLK_SIZE){
	xgl::gl_state = (xgl::GlState*)(memory_block);

	xgl::gl_state->primary_arena.start     = xen::ptrGetAdvanced(xgl::gl_state, sizeof(xgl::GlState));
	xgl::gl_state->primary_arena.next_byte = xgl::gl_state->primary_arena.start;
	xgl::gl_state->primary_arena.end       = xen::ptrGetAdvanced(xgl::gl_state, BLK_SIZE);

	xgl::gl_state->pool_mesh          = xen::createArenaPool<xgl::MeshGlData>(xgl::gl_state->primary_arena, 128);
	xgl::gl_state->pool_texture       = xen::createArenaPool<xen::gl::TextureImpl>(xgl::gl_state->primary_arena, 128);
	xgl::gl_state->pool_render_target = xen::createArenaPool<xen::gl::RenderTargetImpl*>(xgl::gl_state->primary_arena, 128);
}

namespace {
	void* init(xen::Kernel& kernel, const void* params){

		constexpr u64 BLK_SIZE = xen::megabytes(1);

		void* memory_block = xen::allocate(kernel, BLK_SIZE, alignof(xgl::GlState));

		doGlStateInit(memory_block, BLK_SIZE);

		return xgl::gl_state;
	}

	void shutdown(xen::Kernel& kernel){
		// :TODO: should also free gl resources
		xen::deallocate(kernel, xgl::gl_state);
	}

	void pushOp(const xen::RenderOp& op){
		switch(op.type){
		case xen::RenderOp::CLEAR:
			xgl::clearTarget(op.clear.target, op.clear.color);
			break;
		case xen::RenderOp::DRAW:
			xgl::render(op.draw.target, op.draw.viewport, *op.draw.params, op.draw.commands);
			break;
		case xen::RenderOp::SWAP_BUFFERS:
			xgl::swapBuffers(op.swap_buffers.window);
			break;
		}
	}

	void* load(xen::Kernel& kernel, void* data, const void* params){
		xgl::gl_state = (xgl::GlState*)data;


		xgl::gl_state->api.createWindow            = &xgl::createWindow;
		xgl::gl_state->api.destroyWindow           = &xgl::destroyWindow;

		xgl::gl_state->api._createMeshFromMeshData = &xgl::createMesh;
		xgl::gl_state->api.destroyMesh             = &xgl::destroyMesh;

		xgl::gl_state->api._updateMeshVertexData   = &xgl::updateMeshVertexData;

		xgl::gl_state->api.createTexture           = &xgl::createTexture;
		xgl::gl_state->api.destroyTexture          = &xgl::destroyTexture;

		xgl::gl_state->api.pushOp                  = &pushOp;

		return &xgl::gl_state->api;
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
	&tick,
};



#endif
