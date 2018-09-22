////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Raytracer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACERDEVICE_CPP
#define XEN_SREN_RAYTRACERDEVICE_CPP

#include <xen/sren/render-debug.hxx>
#include <xen/sren/rasterizer3d.hxx> // fall back to rasterizer for lines and points
#include "RaytracerDevice.hxx"

#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/sren/RaytracerDevice.hpp>
#include <xen/sren/FragmentShader.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/matrix.hpp>
#include <xen/core/array.hpp>

#include <cstring>

xsr::RaytracerDevice::~RaytracerDevice(){
	this->mesh_store.destroyAllMeshes();
	xen::destroyArenaLinear(*main_allocator, render_scratch_arena);
}

xsr::RaytracerDevice::RaytracerDevice(xen::Array<xsr::PostProcessor*> post_processors)
	: SoftwareDeviceBase(post_processors),
	  render_scratch_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(8))),
	  mesh_store(main_allocator)
{
	// no-op
}

xen::Mesh xsr::RaytracerDevice::createMesh(const xen::MeshData* mesh_data) {
	return this->mesh_store.createMesh(mesh_data);
}
void xsr::RaytracerDevice::destroyMesh(xen::Mesh mesh) {
	this->mesh_store.destroyMesh(mesh);
}
void xsr::RaytracerDevice::updateMeshVertexData(xen::Mesh mesh,
                                                      u32   attrib_index,
                                                      void* new_data,
                                                      u32   start_vertex,
                                                      u32   end_vertex) {
	this->mesh_store.updateMeshVertexData(mesh,
	                                      attrib_index,
	                                      new_data,
	                                      start_vertex,
	                                      end_vertex);
}

void xsr::RaytracerDevice::render(xen::RenderTarget target_handle,
                                  const xen::Aabb2u& viewport,
                                  const xen::RenderParameters3d& params,
                                  const xen::Array<xen::RenderCommand3d> commands
                                 ) {
	xen::resetArena(render_scratch_arena);

	xsr::RenderTarget& target = *this->getRenderTargetImpl(target_handle);

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

	xsr::RaytracerScene scene;
	scene.models.size         = 0;
	scene.first_shadow_caster = 0;
	scene.models.elements     =
		(xsr::RaytracerModel*)xen::ptrGetAlignedForward(render_scratch_arena.next_byte,
		                                                      alignof(xsr::RaytracerModel)
		                                                      );

	for(u32 i = 0; i < xen::size(commands); ++i){
		const xen::RenderCommand3d* cmd = &commands[i];

		switch(cmd->primitive_type){
		case xen::PrimitiveType::TRIANGLES: {
			xsr::RaytracerModel* model = &scene.models[scene.models.size];
			++scene.models.size;

			if(cmd->flags & xen::RenderCommand3d::Flags::DisableShadowCast){
				// We want to sort the scene so all non-shadow casters are
				// at the front of the array
				xsr::RaytracerModel* model_swap = &scene.models[scene.first_shadow_caster];
				++scene.first_shadow_caster;
				*model = *model_swap;
				model = model_swap;
			}

			model->mesh             = this->mesh_store.getMesh(cmd->mesh);
			model->color            = cmd->color;
			model->emissive_color   = cmd->emissive_color;
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
	                sizeof(xsr::RaytracerModel) * scene.models.size);

	this->doRender(target, viewport, params, commands, non_triangle_cmds, scene);
}

namespace xen {
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena,
	                                      xen::Array<xsr::PostProcessor*> post_processors){
		return xen::emplace<xsr::RaytracerDevice>(arena, post_processors);
	}
}

#endif
