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
#include "rasterizer3d.hxx" // fall back to rasterizer for lines and points
#include "RaytracerDevice.hxx"

#include <xen/sren/SoftwareDevice.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/matrix.hpp>
#include <xen/core/array.hpp>

#include <cstring>

class RaytracerDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::sren::MeshStore<xen::sren::RaytracerMesh> mesh_store;

	///< \brief Scratch space for per render call temporary data
	xen::ArenaLinear                         render_scratch_arena;
public:
	~RaytracerDevice(){
		this->mesh_store.destroyAllMeshes();
		xen::destroyArenaLinear(*main_allocator, render_scratch_arena);
	}

	RaytracerDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors),
		  mesh_store(this, main_allocator),
		  render_scratch_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(8)))
	{
		// no-op
	}

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override{
		return this->mesh_store.createMesh(mesh_data);
	}
	void      destroyMesh         (xen::Mesh mesh) override{
		this->mesh_store.destroyMesh(mesh);
	}
	void      updateMeshAttribData(xen::Mesh mesh,
	                               u32   attrib_index,
	                               void* new_data,
	                               u32   start_vertex,
	                               u32   end_vertex) override{
		this->mesh_store.updateMeshAttribData(mesh,
		                                      attrib_index,
		                                      new_data,
		                                      start_vertex,
		                                      end_vertex);
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
		scene.models.size         = 0;
		scene.first_shadow_caster = 0;
		scene.models.elements     =
			(xen::sren::RaytracerModel*)xen::ptrGetAlignedForward(render_scratch_arena.next_byte,
			                                                      alignof(xen::sren::RaytracerModel)
			                                                     );

		for(u32 i = 0; i < xen::size(commands); ++i){
			const xen::RenderCommand3d* cmd = &commands[i];

			switch(cmd->primitive_type){
			case xen::PrimitiveType::TRIANGLES: {
				xen::sren::RaytracerModel* model = &scene.models[scene.models.size];
				++scene.models.size;

				if(cmd->flags & xen::RenderCommand3d::Flags::DisableShadowCast){
					// We want to sort the scene so all non-shadow casters are
					// at the front of the array
					xen::sren::RaytracerModel* model_swap = &scene.models[scene.first_shadow_caster];
					++scene.first_shadow_caster;
					*model = *model_swap;
					model = model_swap;
				}

				model->mesh             = this->mesh_store.getMesh(cmd->mesh);
				model->color            = cmd->color;
				model->emissive_color   = cmd->color;
				model->model_matrix     = cmd->model_matrix;
				model->inv_model_matrix = xen::getInverse(cmd->model_matrix);
				model->aabb_world       = xen::getTransformed(model->mesh->bounds, cmd->model_matrix);
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

		this->doRender(target, viewport, params, commands, non_triangle_cmds, scene);
	};

	void doRender(xen::sren::RenderTargetImpl&           target,
	              const xen::Aabb2u&                     viewport,
	              const xen::RenderParameters3d&         params,
	              const xen::Array<xen::RenderCommand3d> commands,
	              const xen::Array<u32>                  non_triangle_cmds,
	              const xen::sren::RaytracerScene&       scene){

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
		// Render debug of the triangle meshes bounding boxes
		#if 0
		for(u32 i = 0; i < xen::size(scene.models); ++i){
			Mat4r m_matrix = (xen::Scale3d      (xen::getSize(scene.models[i].aabb_world)) *
			                  xen::Translation3d(scene.models[i].aabb_world.min)
			                 );
			rasterizeLinesModel(target, view_region, params,
			                    m_matrix, vp_matrix, xen::Color::RED4f,
			                    xen::TestMeshGeometry_UnitCubeLines.position,
			                    xen::TestMeshGeometry_UnitCubeLines.color,
			                    xen::TestMeshGeometry_UnitCubeLines.vertex_count,
			                    2); //advance by 2 vertex for each line drawn
		}
		#endif
		////////////////////////////////////////////////////////////////////////////

		////////////////////////////////////////////////////////////////////////////
		// Render the non triangles in the scene
		for(u32 i = 0; i < xen::size(commands); ++i){
			u32 cmd_index = non_triangle_cmds[i];
			const xen::RenderCommand3d* cmd = &commands[cmd_index];

			xen::Color4f base_color = cmd->color;
			base_color.rgb *= params.ambient_light;

			const xen::sren::RaytracerMesh* mesh = this->mesh_store.getMesh(cmd->mesh);

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
