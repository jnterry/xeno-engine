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

#include <xen/core/intrinsics.hpp>
#include <xen/graphics/GraphicsDevice.hpp>

#include "Window.hxx"

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

	Mesh GraphicsDevice::createMesh(const MeshGeometrySource& mesh_geom,
	                                const VertexSpec&         vertex_spec){

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
