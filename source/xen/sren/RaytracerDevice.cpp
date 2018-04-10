////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Raytracer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACERDEVICE_CPP
#define XEN_SREN_RAYTRACERDEVICE_CPP

#include "SoftwareDeviceBase.hxx"
#include "render-utilities.hxx"
#include "raytracer3d.hxx"
#include "rasterizer3d.hxx" // fall back to rasterizer for lines and points

#include <xen/sren/SoftwareDevice.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/array.hpp>

#include <cstring>

class RaytracerDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::Allocator*                          mesh_allocator;
	xen::ArenaPool<xen::sren::RaytracerMesh> mesh_pool;

	///< \brief Scratch space for per render call data
	xen::ArenaLinear                         render_scratch_arena;
public:
	~RaytracerDevice(){
		// :TODO: destroy all created meshes

		// :TODO: why is one of these a pointer and one not???? Make consistent...
		//   -> create equivalents also affected
		xen::destroyArenaLinear(*main_allocator, render_scratch_arena);
		xen::destroyArenaPool  ( main_allocator, mesh_pool);
	}

	RaytracerDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors),
		  mesh_allocator(main_allocator),
		  mesh_pool(xen::createArenaPool<xen::sren::RaytracerMesh>(main_allocator, 1024)),
		  render_scratch_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(8)))
	{
		// no-op
	}

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override {
		// :TODO:COMP:ISSUE_31: object pool with automatic handles / resizeable pool
		u32 slot = xen::reserveSlot(this->mesh_pool);
		xen::sren::RaytracerMesh* mesh_geom = &this->mesh_pool.slots[slot].item;

		// Copy over the common elements
		*((xen::MeshHeader*)mesh_geom) = *((xen::MeshHeader*)mesh_data);

		// Allocate storage and copy over attributes, this is equivalent
		// to uploading to the gpu in a gl device
		xen::fillMeshAttribArrays(mesh_geom, mesh_data, mesh_allocator);
		mesh_geom->vertex_count = mesh_data->vertex_count;

		////////////////////////////////////////////////////////////////////////////
		// Compute face normals
		if(mesh_geom->normal == nullptr && mesh_data->vertex_count % 3 == 0){
			for(u32 i = 0; i < mesh_geom->vertex_count; i += 3){
				mesh_geom->normal = (Vec3r*)mesh_allocator->allocate
					(sizeof(Vec3r) * mesh_geom->vertex_count);

				xen::Triangle3r* tri = (xen::Triangle3r*)&mesh_geom->position[i];
				Vec3r normal = xen::computeNormal(*tri);
				mesh_geom->normal[i+0] = normal;
				mesh_geom->normal[i+1] = normal;
				mesh_geom->normal[i+2] = normal;
			}
		}

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
		xen::sren::RaytracerMesh* mesh = &this->mesh_pool.slots[mesh_handle._id].item;

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
			if(start_vertex != 0 || end_vertex != mesh->vertex_count){
				// :TODO: log
				printf("WARN: Updating mesh attrib %i but there is no existing data and "
				       "the new data set is not complete\n", attrib_index);
			}
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
		xen::resetArena(render_scratch_arena);

		xen::sren::RenderTargetImpl& target = *this->getRenderTargetImpl(target_handle);

		////////////////////////////////////////////////////////////////////////////
		// Build up the RaytracerScene by consolidating all triangle drawing
		// commands

		// List of cmd indices referring to non-triangles
		// Worst case is that every command is a non-triangle,
		// so reserve that much space
		xen::Array<u32> non_triangle_cmds;
		non_triangle_cmds.size     = 0;
		non_triangle_cmds.elements = xen::reserveTypeArray<u32>(render_scratch_arena,
		                                                        xen::size(commands)
		                                                       );

		xen::sren::RaytracerScene scene;
		scene.models.size     = 0;
		scene.models.elements =
			(xen::sren::RaytracerModel*)xen::ptrGetAlignedForward(render_scratch_arena.next_byte,
			                                                      alignof(xen::sren::RaytracerModel)
			                                                     );

		for(u32 i = 0; i < xen::size(commands); ++i){
			const xen::RenderCommand3d* cmd = &commands[i];

			switch(cmd->primitive_type){
			case xen::PrimitiveType::TRIANGLES: {
				xen::sren::RaytracerModel* model = &scene.models[scene.models.size];
				++scene.models.size;

				model->mesh             = &this->mesh_pool.slots[cmd->mesh._id].item;
				model->color            = cmd->color;
				model->emissive_color   = cmd->color;
				model->model_matrix     = cmd->model_matrix;
				model->inv_model_matrix = xen::getInverse(cmd->model_matrix);

				break;
			}
			default:
				non_triangle_cmds[non_triangle_cmds.size] = i;
				++non_triangle_cmds.size;
				break;
			}
		}

		xen::ptrAdvance(&render_scratch_arena.next_byte,
		                sizeof(xen::sren::RaytracerModel) * scene.models.size);

		////////////////////////////////////////////////////////////////////////////
		// Render the triangles in the scene
		xen::sren::renderRaytrace(target, viewport, params, scene);

		////////////////////////////////////////////////////////////////////////////
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

		////////////////////////////////////////////////////////////////////////////
		// Render the non triangles in the scene
		for(u32 i = 0; i < xen::size(commands); ++i){
			u32 cmd_index = non_triangle_cmds[i];
			const xen::RenderCommand3d* cmd = &commands[cmd_index];

			xen::Color4f base_color = cmd->color;
			base_color.rgb *= params.ambient_light;

			const xen::sren::RaytracerMesh* mesh = &this->mesh_pool.slots[cmd->mesh._id].item;

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
			default:
				XenInvalidCodePath("Unhandled render command type in rasterizer device");
				break;
			}
		}

		// :TODO: log trace
		//{
		//	u64 used = xen::getBytesUsed(render_scratch_arena);
		//	u64 size = xen::getSize     (render_scratch_arena);
		//	printf("Used %li of %li bytes (%f%%) in raytracer scratch space\n",
		//	       used, size, (float)used / (float)size);
		//}
	}
};

namespace xen {
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena,
	                                      xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<RaytracerDevice>(arena, post_processors);
	}
}

#endif
