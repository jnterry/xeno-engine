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

#include <xen/sren/SoftwareDevice.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/array.hpp>

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
		  mesh_pool(xen::createArenaPool<xen::MeshGeometrySource>(main_allocator, 1024)),
		  render_scratch_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(8)))
	{
		// no-op
	}

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override {
		// :TODO:COMP:ISSUE_31: object pool with automatic handles / resizeable pool
		u32 slot = xen::reserveSlot(this->mesh_pool);
		xen::sren::RaytracerMesh* mesh_geom = &this->mesh_pool.slots[slot].item;

		// Allocate storage and copy over attributes, this is equivalent
		// to uploading to the gpu in a gl device
		xen::fillMeshAttribArrays(mesh_geom, mesh_data, mesh_allocator);
		mesh_geom->vertex_count = mesh_data->vertex_count;

		if(mesh_geom->normal == nullptr){
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

	void updateMeshAttribData(xen::Mesh mesh,
	                          u32 attrib_index,
	                          void* new_data,
	                          u32 start_vertex,
	                          u32 end_vertex
	                          ) {
		// :TODO: implement
	}

	void render(xen::RenderTarget target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xen::resetArena(render_scratch_arena);

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
		// Render the scene
		xen::sren::renderRaytrace(*this->getRenderTargetImpl(target),
		                          viewport,
		                          params,
		                          scene);

		// :TODO: render the non triangle commands
	}
};

namespace xen {
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena,
	                                      xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<RaytracerDevice>(arena, post_processors);
	}
}

#endif
