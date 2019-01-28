////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of exported atom tracer kernel module
///
/// \ingroup module-sren-atom
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENATOM_MODULEATOM_CPP
#define XEN_MODULESRENATOM_MODULEATOM_CPP

#include <ModuleCommon.cpp>
#include "atomtracer.hxx"
#include <xen/math/quaternion.hpp>
#include <xen/math/geometry.hpp>
#include <xen/sren/render-debug.hxx>
#include <xen/kernel/threads.hpp>

xen::ArenaLinear frame_scratch;

void renderDebug(xen::ArenaLinear&  scratch,
                 real               time,
                 xen::RenderTarget  target_handle,
                 const xen::Aabb2u& viewport,
                 const xen::RenderParameters3d& params,
                 const xen::Array<xen::RenderCommand3d> commands
                ) {
	xsr::RenderTarget& target = *xsr::getRenderTargetImpl(target_handle);

	xen::MemoryTransaction transaction(scratch);

	xen::RenderParameters3d test_params = params;

	xen::Angle cycle = time * 60_deg;

	real cam_dist = ((xen::sin(cycle * 0.43_r) + 1.0_r) * 2.0_r) + 1.5_r;
	test_params.camera.position   = xen::rotated (Vec3r{cam_dist, 0, 0}, Vec3r::UnitY, cycle);
	test_params.camera.position.y = xen::sin(cycle * 0.6_r);

	test_params.camera.look_dir = xen::normalized(-test_params.camera.position);

	xen::sren::AtomScene& ascene = xen::sren::atomizeScene(viewport, test_params,
	                                                       commands, scratch,
	                                                       3.0f);

	//////////////////////////////////////////////////////
	// Render the actual atoms
	xsr::RasterizationContext cntx;
	xen::clearToZero(&cntx);
	xen::Aabb2r viewport_r = xen::cast<xen::Aabb2r>(viewport);
	cntx.m_matrix        = Mat4r::Identity; // points already in world space
	cntx.vp_matrix       = xen::getViewProjectionMatrix(params.camera, viewport);
	cntx.viewport        = &viewport_r;
	cntx.fragment_shader = xsr::FragmentShader_DiffuseColor;
	cntx.target          = &target;
	cntx.diffuse_color   = xen::Color::WHITE4f;

	#if 0
	xsr::rasterizePointsModel(cntx,
	                          ascene.positions,
	                          nullptr,
	                          ascene.atom_count
	                         );
	#else
	for(u32 i = 0; i < ascene.boxes.size; ++i){
		if(ascene.boxes[i].end <= ascene.boxes[i].start ||
		   ascene.boxes[i].start >= ascene.atom_count   ||
		   ascene.boxes[i].end   >= ascene.atom_count ){
			// Then this box is empty, skip it
			continue;
		}
		switch(i % 6){
		case 0: cntx.diffuse_color = {1.0f, 0.0f, 0.0f, 1.0f}; break;
		case 1: cntx.diffuse_color = {0.0f, 1.0f, 0.0f, 1.0f}; break;
		case 2: cntx.diffuse_color = {0.0f, 0.0f, 1.0f, 1.0f}; break;

		case 3: cntx.diffuse_color = {1.0f, 0.0f, 1.0f, 1.0f}; break;
		case 4: cntx.diffuse_color = {1.0f, 1.0f, 0.0f, 1.0f}; break;
		case 5: cntx.diffuse_color = {0.0f, 1.0f, 1.0f, 1.0f}; break;
		}
		xsr::rasterizePointsModel(cntx,
		                          &ascene.positions[ascene.boxes[i].start],
		                          nullptr,
		                          ascene.boxes[i].end - ascene.boxes[i].start
		                         );
	}
	#endif

	#if 0
	//////////////////////////////////////////////////////
	// Render bounding boxes of occupied nodes of the oct-tree
	for(u32 i = 0; i < ascene.boxes.size; ++i){
		if(ascene.boxes[i].start < ascene.boxes[i].end){
			xsr::renderDebugBoundingBox(target, viewport, params.camera,
			                            ascene.boxes[i].bounds);
		}
	}
	#endif

	//////////////////////////////////////////////////////
	// Render where the virtual debug camera is located
	xsr::renderCameraDebug(target, viewport,
	                       params.camera, test_params.camera, 2);
}

void render(xen::ArenaLinear& scratch,
            xen::RenderTarget target_handle,
            const xen::Aabb2u& viewport,
            const xen::RenderParameters3d& params,
            const xen::Array<xen::RenderCommand3d> commands
            ) {
	xsr::RenderTarget& target = *xsr::getRenderTargetImpl(target_handle);

	xen::MemoryTransaction transaction(scratch);

	xen::sren::AtomScene& ascene = xen::sren::atomizeScene(viewport,
	                                                       params, commands,
	                                                       scratch,
	                                                       2.0_r
	                                                      );

	xen::sren::computeLighting(ascene, scratch, params);

	xen::sren::rasterizeAtoms(target, viewport, params, ascene);
}

void tick(const xen::TickContext& cntx){
	for(u32 i = 0; i < xsr::state->next_free_op; ++i){
		xen::RenderOp& op = xsr::state->op_list[i];

		switch(op.type){
		case xen::RenderOp::CLEAR:
			xsr::clear(op.clear.target, op.clear.color);
			break;
		case xen::RenderOp::DRAW:
			#if 1
			render(frame_scratch,
			       op.draw.target, op.draw.viewport,
			       *op.draw.params, op.draw.commands);
			#else
			renderDebug(frame_scratch,
			            xen::asSeconds<real>(cntx.time),
			            op.draw.target, op.draw.viewport,
			            *op.draw.params, op.draw.commands);
			#endif
			break;
		case xen::RenderOp::SWAP_BUFFERS:
			xsr::swapBuffers(op.swap_buffers.window);
			break;
		}
	}
	xsr::state->next_free_op = 0;
}

void* atomLoad(void* data, const void* params){
	const u64 FRAME_SCRATCH_SIZE = xen::megabytes(512);

	frame_scratch.start = xen::kernelAlloc(FRAME_SCRATCH_SIZE);
	frame_scratch.end   = xen::ptrGetAdvanced(frame_scratch.start, FRAME_SCRATCH_SIZE);
	frame_scratch.next_byte = frame_scratch.start;

	return load(data, params);
}

void unload(void* data, const void* params){
	xen::kernelFree(frame_scratch.start);
}

XenDeclareModule("graphics", &init, &shutdown, &atomLoad, &unload, &tick)

#endif
