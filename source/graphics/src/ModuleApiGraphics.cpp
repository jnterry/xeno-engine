////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API agnositc implementation of GraphicsModuleApi
/// related functions
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSMODULEAPI_CPP
#define XEN_GRAPHICS_GRAPHICSMODULEAPI_CPP

#include <xen/window/Window.hxx> // :TODO: remove
#include <xen/graphics/ModuleApiGraphics.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/intrinsics.hpp>

#include <cstdarg>

xen::RenderOp xen::RenderOp::Clear(xen::RenderTarget target, xen::Color color){
	xen::RenderOp result;
	result.type         = xen::RenderOp::CLEAR;
	result.clear.target = target;
	result.clear.color  = color;
	return result;
}

xen::RenderOp xen::RenderOp::Draw(xen::RenderTarget target, xen::Aabb2u viewport,
                    xen::RenderParameters3d& params, xen::Array<RenderCommand3d>& commands
                   ){
	xen::RenderOp result;
	result.type          = xen::RenderOp::DRAW;
	result.draw.target   = target;
	result.draw.viewport = viewport;
	result.draw.params   = &params;
	result.draw.commands = commands;
	return result;
}

xen::RenderOp xen::RenderOp::SwapBuffers(xen::RenderTarget target){
	xen::RenderOp result;
	result.type                = xen::RenderOp::SWAP_BUFFERS;
	result.swap_buffers.target = target;
	return result;
}

namespace xen {
	Mesh ModuleApiGraphics::createMesh(const VertexSpec& vertex_spec,
	                                   const xen::PrimitiveType primitive_type,
	                                   u32 vertex_count,
	                                   ...){



		void* attrib_data[255]; // Can only have up to 255 attributes in mesh

		MeshData md;

		md.vertex_spec    = vertex_spec;
		md.primitive_type = primitive_type;
		md.vertex_count   = vertex_count;
		md.vertex_data    = attrib_data;

		Vec3r* pbuf = nullptr;

		va_list args;
		va_start(args, vertex_count);
		for(u32 i = 0; i < xen::size(vertex_spec); ++i){
			attrib_data[i] = va_arg(args, void*);
			if(vertex_spec[i] == xen::VertexAttribute::Position3r){
				pbuf = (Vec3r*)attrib_data[i];
			}
		}
		va_end(args);

		XenAssert(pbuf != nullptr, "Expected mesh to have position data");

		md.bounds = xen::computeBoundingBox(pbuf, vertex_count);

		/////////////////////////////////////////////////////////////////

		return this->createMesh(&md);
	}
}

void xen::ModuleApiGraphics::clear(xen::RenderTarget target, xen::Color color){
	this->pushOp(xen::RenderOp::Clear(target, color));
}

void xen::ModuleApiGraphics::render(xen::RenderTarget target,
                                    xen::Aabb2u viewport,
                                    xen::RenderParameters3d& params,
                                    xen::Array<RenderCommand3d> commands
                                   ){
	this->pushOp(xen::RenderOp::Draw(target, viewport, params, commands));
}

void xen::ModuleApiGraphics::render(xen::RenderTarget target,
                                    xen::Aabb2u viewport,
                                    xen::RenderParameters3d& params,
                                    xen::RenderCommand3d& cmd){
	xen::Array<xen::RenderCommand3d> cmd_list = { 1, &cmd };
	this->pushOp(xen::RenderOp::Draw(target, viewport, params, cmd_list));
}

void xen::ModuleApiGraphics::swapBuffers(xen::RenderTarget target){
	this->pushOp(xen::RenderOp::SwapBuffers(target));
}

#endif
