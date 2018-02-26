////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the OpenGL Graphics Device
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_GLDEVICE_CPP
#define XEN_GL_GLDEVICE_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/VertexAttribute.hpp>

#include "gl_header.hxx"
#include "Mesh.hxx"

#include <utility>

static const constexpr u32 MESH_STORE_SIZE = 128;

class GlDevice : public xen::GraphicsDevice {
private:
	xen::Allocator* main_allocator;

	xen::FixedArray<xen::gl::MeshHeader*, MESH_STORE_SIZE> mesh_store;
	xen::ArenaLinear mesh_header_arena;
public:
	~GlDevice(){
		// no-op
	}

	GlDevice()
		: GraphicsDevice(xen::GraphicsDeviceType::OpenGL),
		  main_allocator(new xen::AllocatorCounter<xen::AllocatorMalloc>()),
		  mesh_header_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(1)))
	{

		// :TODO: better way of managing mesh headers:
		// - remove fixed limit MESH_STORE_SIZE
		// - deal with mesh generations (IE: re-using a mesh id)
		// - mesh_header_arena will currently just slowly fill up, need way to
		//   reclaim space when a mesh is deleted
		for(u32 i = 0; i < MESH_STORE_SIZE; ++i){
			mesh_store[i] = nullptr;
		}
	}

	xen::Mesh createMesh(xen::MeshData& mesh_data){
		u32 slot;
		for(slot = 0; slot < MESH_STORE_SIZE; ++slot){
			if(mesh_store[slot] == nullptr){
				break;
			}
		}

		XenAssert(slot < MESH_STORE_SIZE, "Mesh store full, cannot create new mesh");

		mesh_store[slot] = xen::gl::createMesh(mesh_header_arena,
		                                       mesh_data.attrib_count,
		                                       mesh_data.attrib_types,
		                                       mesh_data.attrib_data,
		                                       mesh_data.vertex_count
		                                      );

		return makeHandle<xen::Mesh::HANDLE_ID>(slot, 0);

	}

	void destroyMesh(xen::Mesh mesh) {
		// :TODO: IMPLEMENT - currently resource link, GPU buffers needs destroying
		mesh_store[mesh._id] = nullptr;
	}

	void clear(xen::RenderTarget target,
	           const xen::Aabb2u& viewport,
	           xen::Color color
	           ) override {

	}

	void render(xen::RenderTarget target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	           ) {

	}
		/// @}
};

namespace xen {



}

#endif
