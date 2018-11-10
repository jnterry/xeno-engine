////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of exported rasterizer kernel module
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP
#define XEN_MODULESRENRASTERIZE_MODULERASTERIZE_CPP

#include <ModuleCommon.cpp>
#include "raytracer3d.hxx"
#include <xen/math/geometry.hpp>
#include <xen/kernel/threads.hpp>
#include <xen/kernel/log.hpp>

// Data passed to a raytracer thread. This is just the parameters to
// xsr::renderRaytracer
struct ThreadRenderData {
	xsr::RenderTarget*     target;
	xen::Aabb2u                      viewport;
	xen::Aabb2u                      rendering_bounds;
	const xen::RenderParameters3d*   params;
	const xsr::RaytracerScene* scene;

};
// Function that should be called by each thread to perform a block of raytracing
void threadDoRenderWork(void* voiddata){
	ThreadRenderData* data = (ThreadRenderData*)voiddata;
	xsr::renderRaytrace(*data->target,
	                          data->viewport,
	                          *data->params,
	                          *data->scene,
	                          data->rendering_bounds);
}

void doRender(xsr::RenderTarget&                     target,
              const xen::Aabb2u&                     viewport,
              const xen::RenderParameters3d&         params,
              const xen::Array<xen::RenderCommand3d> commands,
              const xen::Array<u32>                  non_triangle_cmds,
              const xsr::RaytracerScene&             scene){

	////////////////////////////////////////////////////////////////////////////
	// Render the triangles in the scene
	xen::TickWorkHandle work_group = xen::createTickWorkGroup();
	ThreadRenderData thread_render_data;
	constexpr u32 WORK_DIVISIONS = 16;
	u32 cur_y   = viewport.min.x;
	u32 delta_y = (viewport.max.y - viewport.min.y) / WORK_DIVISIONS;
	for(u32 i = 0; i < WORK_DIVISIONS; ++i){
		thread_render_data.target        = &target;
		thread_render_data.params        = &params;
		thread_render_data.scene         = &scene;
		thread_render_data.viewport = viewport;

		thread_render_data.rendering_bounds.min.x = viewport.min.x;
		thread_render_data.rendering_bounds.max.x = viewport.max.x;
		thread_render_data.rendering_bounds.min.y = cur_y;
		thread_render_data.rendering_bounds.max.y = cur_y + delta_y;
		cur_y += delta_y;

		if(i == WORK_DIVISIONS - 1){
			// The last block of work may have a few extra rows if the viewport
			// height is not divisible by the number of work blocks
			thread_render_data.rendering_bounds.max.y = viewport.max.y;
		}

		xen::pushTickWork(&threadDoRenderWork, &thread_render_data, work_group);
	}
	xen::waitForTickWork(work_group);

	////////////////////////////////////////////////////////////////////////////
	// Generate view projection matrix
	if(!xen::isCameraValid(params.camera)){
		XenLogWarn("Camera is not valid, skipping rendering");
		return;
	}

	// Find the actual view_region we wish to draw to. This is the
	// intersection of the actual target, and the user specified viewport
	xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
	xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

	Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

	if(xen::isnan(vp_matrix)){
	  XenLogWarn("vp_matrix contains NaN elements, skipping rendering");
		return;
	}

	////////////////////////////////////////////////////////////////////////////
	// Render debug of the triangle meshes bounding boxes
	#if 0
	for(u32 i = 0; i < xen::size(scene.models); ++i){
		xen::sren::renderBoundingBox(target, view_region, vp_matrix,
		                             scene.models[i].aabb_world, xen::Color::RED4f);
	}
	#endif
	////////////////////////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////////////////////////
	// Render the non triangles in the scene
	xsr::RasterizationContext context;
	xen::clearToZero(&context);
	xen::copyBytes(&params, ((xen::RenderParameters3d*)&context), sizeof(xen::RenderParameters3d));
	context.target      = &target;
	context.viewport    = &view_region;
	context.textures[0] = nullptr;
	context.textures[1] = nullptr;
	context.textures[2] = nullptr;
	context.textures[3] = nullptr;
	for(u32 i = 0; i < xen::size(non_triangle_cmds); ++i){
		u32 cmd_index = non_triangle_cmds[i];
		const xen::RenderCommand3d* cmd = &commands[cmd_index];
		context.fragment_shader = xsr::getShaderImpl(cmd->shader);
		setPerCommandFragmentUniforms(context, *cmd,
		                              cmd->model_matrix, vp_matrix
		                             );
		rasterizeMesh(context, cmd->primitive_type, *xsr::getMeshImpl(cmd->mesh));
	}
}

void render(xen::RenderTarget target_handle,
            const xen::Aabb2u& viewport,
            const xen::RenderParameters3d& params,
            const xen::Array<xen::RenderCommand3d> commands
           ) {
	xen::ArenaLinear& render_scratch_arena = xen::getThreadScratchSpace();

	xsr::RenderTarget& target = *xsr::getRenderTargetImpl(target_handle);

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

			model->mesh             = xsr::getMeshImpl(cmd->mesh);
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

	doRender(target, viewport, params, commands, non_triangle_cmds, scene);

	{
		u64 used = xen::getBytesUsed(render_scratch_arena);
		u64 size = xen::getSize     (render_scratch_arena);
		XenLogDebug("Used %li of %li bytes (%f%%) in raytracer scratch space",
		            used, size, (float)used / (float)size);
	}
}

namespace {
	void tick(const xen::TickContext& tick){
		for(u32 i = 0; i < xsr::state->next_free_op; ++i){
			xen::RenderOp& op = xsr::state->op_list[i];

			switch(op.type){
			case xen::RenderOp::CLEAR:
				xsr::clear(op.clear.target, op.clear.color);
				break;
			case xen::RenderOp::DRAW:
				render(op.draw.target, op.draw.viewport,
				       *op.draw.params, op.draw.commands);
				break;
			case xen::RenderOp::SWAP_BUFFERS:
				xsr::swapBuffers(op.swap_buffers.window);
				break;
			}
		}
		xsr::state->next_free_op = 0;
	}
}

XenDeclareModule("graphics", &init, &shutdown, &load, nullptr, &tick);

#endif
