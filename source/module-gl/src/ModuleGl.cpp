////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Creates and exports the xen::Module instance
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_MODULE_CPP
#define XEN_GL_MODULE_CPP

#include <xen/graphics/ModuleApiGraphics.hpp>
#include <xen/kernel/Module.hpp>
#include <xen/kernel/Kernel.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/String.hpp>

#include "ModuleGl.hxx"
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


xgl::GlState* xgl::state = nullptr;

void* init( const void* params){

	constexpr u64 BLK_SIZE = xen::megabytes(1);

	void* memory_block = xen::kernelAlloc(BLK_SIZE, alignof(xgl::GlState));

	xgl::state = (xgl::GlState*)(memory_block);

	xgl::state->primary_arena.start     = xen::ptrGetAdvanced(xgl::state, sizeof(xgl::GlState));
	xgl::state->primary_arena.next_byte = xgl::state->primary_arena.start;
	xgl::state->primary_arena.end       = xen::ptrGetAdvanced(xgl::state, BLK_SIZE);

	xgl::state->pool_mesh          = xen::createArenaPool<xgl::MeshGlData>(xgl::state->primary_arena, 128);
	xgl::state->pool_texture       = xen::createArenaPool<xgl::Texture>(xgl::state->primary_arena, 128);
	xgl::state->pool_shader        = xen::createArenaPool<xgl::ShaderProgram>(xgl::state->primary_arena, 128);
	xgl::state->pool_material      = xen::createArenaPool<xgl::Material>(xgl::state->primary_arena, 128);
	xgl::state->pool_render_target = xen::createArenaPool<xgl::RenderTargetImpl*>(xgl::state->primary_arena, 128);

	return xgl::state;
}

void shutdown(void* data, const void* params){
	// :TODO: should also free gl resources
	xen::kernelFree(xgl::state);
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
		xgl::swapBuffers(op.swap_buffers.target);
		break;
	}
}

void* load( void* data, const void* params){
	xgl::state = (xgl::GlState*)data;

	xgl::state->api.createWindowRenderTarget = &xgl::createWindowRenderTarget;
	xgl::state->api.destroyRenderTarget      = &xgl::destroyRenderTarget;

	xgl::state->api._createMeshFromMeshData   = &xgl::createMesh;
	xgl::state->api._updateMeshVertexData     = &xgl::updateMeshVertexData;
	xgl::state->api.createDynamicMesh         = &xgl::createDynamicMesh;
	xgl::state->api.setDynamicMeshVertexCount = &xgl::setDynamicMeshVertexCount;
	xgl::state->api.destroyMesh               = &xgl::destroyMesh;

	xgl::state->api._createTexture            = &xgl::createTexture;
	xgl::state->api._updateTexture            = &xgl::updateTextureData;
	xgl::state->api.destroyTexture            = &xgl::destroyTexture;

	xgl::state->api.createMaterial            = &xgl::createMaterial;
	xgl::state->api.destroyMaterial           = &xgl::destroyMaterial;

	xgl::state->api.pushOp                    = &pushOp;

	return &xgl::state->api;
}

void tick(const xen::TickContext& tick){
	xgl::state->kernel_time = xen::asSeconds<real>(tick.time);
}

XenDeclareModule("graphics", &init, &shutdown, &load, nullptr, &tick)

#endif
