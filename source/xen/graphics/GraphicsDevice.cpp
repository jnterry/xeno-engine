////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API agnositc implementation of GraphicsDevice
/// related functions
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSDEVICE_CPP
#define XEN_GRAPHICS_GRAPHICSDEVICE_CPP

#include "Window.hxx"

#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/intrinsics.hpp>

#include <cstdarg>

namespace {
	/// \brief Map from device id to a pointer to the device
	xen::GraphicsDevice* created_devices[256] = {0};

	/// \brief The id of the next device that will be created
	u08 next_device_id = 0;
}

namespace xen {
	GraphicsDevice::GraphicsDevice()
		: id(next_device_id) {

		// :TODO: next_device_id should be incremented atomically user tries
		// creating a device per thread

		XenAssert(next_device_id < 255,
		          "Maximum number of graphics devices have already been created"
		         );

		++next_device_id;

		created_devices[this->id] = this;
	}

	GraphicsDevice::~GraphicsDevice(){
		created_devices[this->id] = nullptr;
	}

	GraphicsDevice* getGraphicsDevice(u08 id){
		return created_devices[id];
	}

	Mesh GraphicsDevice::createMesh(const VertexSpec&         vertex_spec,
	                                const MeshGeometrySource& mesh_geom
	                                ){

		// max number of attribs is 255, so allocate that much stack space
		void* attrib_data[255];

		for(u32 i = 0; i < xen::size(vertex_spec); ++i){
			switch(vertex_spec[i]){
			case xen::VertexAttribute::Position3r:
				attrib_data[i] = mesh_geom.position;
				break;
			case xen::VertexAttribute::Normal3r:
				attrib_data[i] = mesh_geom.normal;
				break;
			case xen::VertexAttribute::Color4b:
				attrib_data[i] = mesh_geom.color;
				break;
			default: break;
			}
		}

		xen::MeshData mesh_data;
		mesh_data.attrib_count = xen::size(vertex_spec);
		mesh_data.attrib_types = vertex_spec.elements;
		mesh_data.vertex_count = mesh_geom.vertex_count;
		mesh_data.attrib_data  = attrib_data;

		return this->createMesh(&mesh_data);
	}

	Mesh GraphicsDevice::createMesh(const VertexSpec& vertex_spec,
	                                u32               vertex_count,
	                                ...){



		void* attrib_data[255]; // Can only have up to 255 attributes in mesh

		MeshData md;

		md.attrib_count = xen::size(vertex_spec);
		md.vertex_count = vertex_count;
		md.attrib_data  = attrib_data;

		// Its fine to do this const cast since we use md as a const later on,
		// an we don't modify the vertex_spec in this function
		md.attrib_types = const_cast<xen::VertexAttribute::Type*>(&vertex_spec[0]);

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

		md.bounds = xen::Aabb3r::MaxMinBox;
		for(u32 i = 0; i < vertex_count; ++i){
			xen::addPoint(md.bounds, pbuf[i]);
		}

		// ensure we use md as const to prevent above const_cast from being invalid
		// if createMesh is ever changed to take non const pointer
		const MeshData* md_const = &md;
		return this->createMesh(md_const);
	}

	void GraphicsDevice::clear(Window* window, xen::Color color){
		if(window->is_open){
			clear(window->render_target, color);
		}
	}

	void GraphicsDevice::render(Window* window,
	                            const xen::Aabb2u& viewport,
	                            const RenderParameters3d& params,
	                            const xen::Array<RenderCommand3d> commands
	                            ){
		if(window->is_open){
			render(window->render_target, viewport, params, commands);
		}
	}
}

#endif
