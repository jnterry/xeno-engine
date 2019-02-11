#include <stdio.h>

#include <xen/kernel/Kernel.hpp>
#include <xen/kernel/Module.hpp>
#include <xen/core/random.hpp>

#include <xen/window/Window.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/ModuleApiGraphics.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/String.hpp>

#define STAR_COUNT 1024

#include "../utilities.hpp"

struct StarfieldState {
	xen::ArenaLinear arena;

	Vec3r       star_positions[STAR_COUNT];
	xen::Color  star_colors   [STAR_COUNT];

	xen::FixedArray<xen::VertexAttribute, 3> vertex_spec;
	const xen::Mesh* mesh_stars;
	const xen::Mesh* mesh_axes;
	const xen::Mesh* mesh_cube_lines;

	xen::Window*                             window;
	xen::RenderTarget                        window_target;

	xen::RenderParameters3d                  render_params;
	xen::Camera3dCylinder                    camera;
	xen::FixedArray<xen::RenderCommand3d, 3> render_cmds;
};

StarfieldState* star_state;

void* init(const void* params){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();
	XenAssert(mod_ren != nullptr, "Graphics module must be loaded before cornell-box");
	XenAssert(mod_win != nullptr, "Window module must be loaded before cornell-box");


	StarfieldState* ss = (StarfieldState*)xen::kernelAlloc(sizeof(StarfieldState) + xen::kilobytes(1));
	ss->arena.start     = xen::ptrGetAdvanced(ss, sizeof(StarfieldState));
	ss->arena.end       = xen::ptrGetAdvanced(ss->arena.start, xen::kilobytes(1));
	ss->arena.next_byte = ss->arena.start;

	ss->window        = mod_win->createWindow({600, 600}, "starfield");
	ss->window_target = mod_ren->createWindowRenderTarget(ss->window);

	ss->camera.z_near = 0.001;
	ss->camera.z_far  = 1000;
	ss->camera.fov_y  = 70_deg;
	ss->camera.radius = 450;
	ss->camera.height = 0;
	ss->camera.up_dir = Vec3r::UnitY;
	ss->camera.axis   = Vec3r::UnitY;
	ss->camera.target = Vec3r::Origin;
	ss->camera.angle  = 0.0_deg;

	for(u32 i = 0; i < STAR_COUNT; ++i){
		ss->star_positions[i].x = xen::randf(-100, 100);
		ss->star_positions[i].y = xen::randf(-100, 100);
		ss->star_positions[i].z = xen::randf(-100, 100);

		ss->star_colors[i].r = xen::mapToRange<real, u08>(-100, 100, 0, 255, ss->star_positions[i].x);
		ss->star_colors[i].g = xen::mapToRange<real, u08>(-100, 100, 0, 255, ss->star_positions[i].y);
		ss->star_colors[i].b = 255; //xen::mapToRange<real, u08>(-100, 100, 0, 255, ss->star_positions[i].z);
		ss->star_colors[i].a = 255;
	}

	xen::clearToZero(&ss->render_params);
	ss->render_params.ambient_light = xen::Color3f(1.0f, 1.0f, 1.0f);

	ss->vertex_spec[0] = xen::VertexAttribute::Position3r;
	ss->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	ss->vertex_spec[2] = xen::VertexAttribute::Color4b;

	ss->mesh_stars      = mod_ren->createMesh(ss->vertex_spec, xen::PrimitiveType::Points, STAR_COUNT,
	                                          ss->star_positions, nullptr, ss->star_colors);

	ss->mesh_axes       = mod_ren->createMesh(xen::TestMeshData_Axes);
	ss->mesh_cube_lines = mod_ren->createMesh(xen::TestMeshData_UnitCubeLines);
	xen::clearToZero(ss->render_cmds);

	ss->render_cmds[0].model_matrix           = xen::Scale3d(100_r);
	ss->render_cmds[0].mesh                   = ss->mesh_axes;

	ss->render_cmds[1].model_matrix           = Mat4r::Identity;
	ss->render_cmds[1].mesh                   = ss->mesh_stars;

	ss->render_cmds[2].model_matrix           = (
		xen::Scale3d(200_r) * xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	);
	ss->render_cmds[2].mesh                   = ss->mesh_cube_lines;

	return ss;
}

void shutdown(void* data, const void* params){
	xen::kernelFree(star_state);
	star_state = nullptr;
}

void tick( const xen::TickContext& cntx){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();

	xen::Aabb2u viewport = { Vec2u::Origin, mod_win->getClientAreaSize(star_state->window) };

	xen::WindowEvent* event;
	while((event = mod_win->pollEvent(star_state->window))){
		switch(event->type){
		case xen::WindowEvent::Closed:
			mod_win->destroyWindow(star_state->window);
			xen::requestKernelShutdown();
			break;
		default: break;
		}
	}

	handleCameraInputCylinder(mod_win, star_state->window, star_state->camera, xen::asSeconds<real>(cntx.dt));
	star_state->render_params.camera = xen::generateCamera3d(star_state->camera);

	for(u32 i = 0; i < STAR_COUNT; ++i){
		star_state->star_positions[i].z += xen::asSeconds<real>(cntx.dt) * 100.0f;
		if(star_state->star_positions[i].z >= 100.0f){
			star_state->star_positions[i].z -= 200.0f;
		}
	}
	mod_ren->updateMeshVertexData(star_state->mesh_stars, 0, star_state->star_positions);

	mod_ren->clear      (star_state->window_target, xen::Color{20,20,20,255});
	mod_ren->render     (star_state->window_target, viewport,
	                     star_state->render_params, star_state->render_cmds);
	mod_ren->swapBuffers(star_state->window_target);
}

void* load( void* data, const void* params){
	star_state = (StarfieldState*)data;

	return (void*)true;
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
