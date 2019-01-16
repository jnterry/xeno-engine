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

#include <xen/graphics/Window.hxx>
#include <xen/graphics/GraphicsModuleApi.hpp>
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

xen::RenderOp xen::RenderOp::SwapBuffers(xen::Window* window){
	xen::RenderOp result;
	result.type                = xen::RenderOp::SWAP_BUFFERS;
	result.swap_buffers.window = window;
	return result;
}

namespace xen {
	Mesh GraphicsModuleApi::createMesh(const VertexSpec&         vertex_spec,
	                                   const MeshAttribArrays& mesh_geom
	                                  ){

		// max number of vertex attributes is 255, so allocate that much stack space
		XenAssert(vertex_spec.size < 255, "Can only support up to 255 vertex attributes");
		void* vertex_data[255];

		/////////////////////////////////////////////////////////////////
		// Construct attrib data array based on vertex_spec and what we
		// have available in mesh_geom
		for(u32 i = 0; i < xen::size(vertex_spec); ++i){
			switch(vertex_spec[i]){
			case xen::VertexAttribute::Position3r:
				vertex_data[i] = mesh_geom.position;
				break;
			case xen::VertexAttribute::Normal3r:
				vertex_data[i] = mesh_geom.normal;
				break;
			case xen::VertexAttribute::Color4b:
				vertex_data[i] = mesh_geom.color;
				break;
			case xen::VertexAttribute::TexCoord2f:
				vertex_data[i] = mesh_geom.uvs;
			default: break;
			}
		}

		/////////////////////////////////////////////////////////////////
		// Set mesh data fields
		xen::MeshData mesh_data;
		mesh_data.vertex_spec  = vertex_spec;
		mesh_data.vertex_data  = vertex_data;
		mesh_data.vertex_count = mesh_geom.vertex_count;

		/////////////////////////////////////////////////////////////////
		// Compute mesh bounding box
		mesh_data.bounds = xen::Aabb3r::MaxMinBox;
		for(u32 i = 0; i < mesh_geom.vertex_count; ++i){
			xen::addPoint(mesh_data.bounds, mesh_geom.position[i]);
		}

		return this->createMesh(&mesh_data);
	}

	Mesh GraphicsModuleApi::createMesh(const VertexSpec& vertex_spec,
	                                   u32               vertex_count,
	                                   ...
	                                  ){



		void* attrib_data[255]; // Can only have up to 255 attributes in mesh

		MeshData md;

		md.vertex_spec  = vertex_spec;
		md.vertex_count = vertex_count;
		md.vertex_data  = attrib_data;

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

		/////////////////////////////////////////////////////////////////
		// Compute mesh bounding box
		md.bounds = xen::Aabb3r::MaxMinBox;
		for(u32 i = 0; i < vertex_count; ++i){
			xen::addPoint(md.bounds, pbuf[i]);
		}
		/////////////////////////////////////////////////////////////////

		return this->createMesh(&md);
	}
}

void xen::GraphicsModuleApi::clear(xen::RenderTarget target, xen::Color color){
	this->pushOp(xen::RenderOp::Clear(target, color));
}

void xen::GraphicsModuleApi::clear(xen::Window* window, xen::Color color){
	if(window->is_open){
		this->clear(window->render_target, color);
	}
}


void xen::GraphicsModuleApi::render(xen::RenderTarget target,
                                    xen::Aabb2u viewport,
                                    xen::RenderParameters3d& params,
                                    xen::Array<RenderCommand3d> commands
                                   ){
	this->pushOp(xen::RenderOp::Draw(target, viewport, params, commands));
}

void xen::GraphicsModuleApi::render(xen::Window* window,
                                    xen::Aabb2u viewport,
                                    xen::RenderParameters3d& params,
                                    xen::Array<RenderCommand3d> commands
                                   ){
		if(window->is_open){
			this->render(window->render_target, viewport, params, commands);
		}
	}

void xen::GraphicsModuleApi::swapBuffers(xen::Window* window){
	this->pushOp(xen::RenderOp::SwapBuffers(window));
}

#endif