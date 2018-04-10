////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Rasterizer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZERDEVICE_CPP
#define XEN_SREN_RASTERIZERDEVICE_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/sren/SoftwareDevice.hpp>
#include <xen/math/geometry.hpp>

#include "SoftwareDeviceBase.hxx"
#include "render-utilities.hxx"
#include "rasterizer3d.hxx"

#include <cstring>

class RasterizerDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::Allocator*                         mesh_allocator;
	xen::ArenaPool<xen::sren::RasterizerMesh> mesh_pool;
public:
	~RasterizerDevice(){

	}

	RasterizerDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors),
		mesh_allocator(main_allocator),
		mesh_pool(xen::createArenaPool<xen::sren::RasterizerMesh>(main_allocator, 1024))
	{
		// no-op
	}

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override {
		// :TODO:COMP:ISSUE_31: object pool with automatic handles / resizeable pool
		u32 slot = xen::reserveSlot(this->mesh_pool);
		xen::sren::RasterizerMesh* mesh_geom = &this->mesh_pool.slots[slot].item;

		// Copy over the common elements
		*((xen::MeshHeader*)mesh_geom) = *((xen::MeshHeader*)mesh_data);

		// Allocate storage and copy over attributes, this is equivalent
		// to uploading to the gpu in a gl device
		xen::fillMeshAttribArrays(mesh_geom, mesh_data, mesh_allocator);
		mesh_geom->vertex_count = mesh_data->vertex_count;

		return this->makeHandle<xen::Mesh::HANDLE_ID>(slot, 0);
	}

	void destroyMesh(xen::Mesh mesh) override {
		xen::freeMeshAttribArrays(&this->mesh_pool.slots[mesh._id].item,
		                          mesh_allocator);
		xen::freeSlot(this->mesh_pool, mesh._id);
	}

	void updateMeshAttribData(xen::Mesh mesh_handle,
	                          u32 attrib_index,
	                          void* new_data,
	                          u32 start_vertex,
	                          u32 end_vertex
	                          ) {
		xen::sren::RasterizerMesh* mesh = &this->mesh_pool.slots[mesh_handle._id].item;

        end_vertex = xen::min(end_vertex, mesh->vertex_count);
		if(end_vertex < start_vertex){ return; }

		void** attrib_data = nullptr;
		switch(mesh->attrib_types[attrib_index]){
		case xen::VertexAttribute::Position3r:
			attrib_data = (void**)&mesh->position;
			break;
		case xen::VertexAttribute::Normal3r:
			attrib_data = (void**)&mesh->normal;
			break;
		case xen::VertexAttribute::Color4b:
			attrib_data = (void**)&mesh->color;
			break;
		default:
			XenInvalidCodePath("Attempt to update unsupported mesh attribute type");
			return;
		}

		u32 attrib_size = xen::getVertexAttributeSize(mesh->attrib_types[attrib_index]);
		if(*attrib_data == nullptr){
			*attrib_data = mesh_allocator->allocate(attrib_size * mesh->vertex_count);
		}

		memcpy(xen::ptrGetAdvanced(*attrib_data, start_vertex * attrib_size),
		       new_data,
		       (end_vertex - start_vertex) * attrib_size
		      );
	}

	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xen::sren::RenderTargetImpl& target = *this->getRenderTargetImpl(target_handle);

		//////////////////////////////////////////////////////////////////////////////
		// Generate view projection matrix
		if(!xen::isCameraValid(params.camera)){
			printf("ERROR: Camera is not valid, skipping rendering\n");
			return;
		}

		// Find the actual view_region we wish to draw to. This is the
		// intersection of the actual target, and the user specified viewport
		xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
		xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

		Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

		if(xen::isnan(vp_matrix)){
			// :TODO: log
			printf("ERROR: vp_matrix contains NaN elements, skipping rendering\n");
			return;
		}
		//////////////////////////////////////////////////////////////////////////////

		const xen::RenderCommand3d* cmd;
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			cmd = &commands[cmd_index];

			xen::Color4f base_color = cmd->color;
			base_color.rgb *= params.ambient_light;

			const xen::sren::RasterizerMesh* mesh = &this->mesh_pool.slots[cmd->mesh._id].item;

			/////////////////////////////////////////////////////////////////
			// Do the drawing, based on primitive type
			switch(cmd->primitive_type){
			case xen::PrimitiveType::POINTS:
				rasterizePointsModel(target, view_region, params,
				                     cmd->model_matrix, vp_matrix, cmd->color,
				                     mesh->position,
				                     mesh->color,
				                     mesh->vertex_count);
				break;
			case xen::PrimitiveType::LINES:
				rasterizeLinesModel(target, view_region, params,
				                    cmd->model_matrix, vp_matrix, cmd->color,
				                    mesh->position,
				                    mesh->color,
				                    mesh->vertex_count,
				                    2); //advance by 2 vertices for each line drawn
				break;
			case xen::PrimitiveType::LINE_STRIP:
				rasterizeLinesModel(target, view_region, params,
				                    cmd->model_matrix, vp_matrix, cmd->color,
				                    mesh->position,
				                    mesh->color,
				                    mesh->vertex_count,
				                    1); //advance by 1 vertex for each line drawn
				break;
			case xen::PrimitiveType::TRIANGLES: {
				rasterizeTrianglesModel(target, view_region, params,
				                        cmd->model_matrix, vp_matrix, cmd->color,
				                        mesh->position,
				                        mesh->normal,
				                        mesh->color,
				                        mesh->vertex_count);
				break;
			}
			default:
				XenInvalidCodePath("Unhandled render command type in rasterizer device");
				break;
			}
		}
	}
};

namespace xen {
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<RasterizerDevice>(arena, post_processors);
	}
}

#endif
