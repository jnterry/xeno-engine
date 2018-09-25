#include <stdio.h>

#include <xen/kernel/Kernel.hpp>
#include <xen/kernel/Module.hpp>
#include <xen/core/random.hpp>

#include <xen/graphics/Window.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/GraphicsModuleApi.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/String.hpp>

#define STAR_COUNT 1024

#include "../utilities.hpp"

struct StarfieldState {
	Vec3r       star_positions[STAR_COUNT];
	xen::Color  star_colors   [STAR_COUNT];

	xen::FixedArray<xen::VertexAttribute::Type, 2> vertex_spec;
	xen::Mesh mesh_stars;
	xen::Mesh mesh_axes;
	xen::Mesh mesh_cube_lines;

	xen::Window*                             window;

	xen::RenderParameters3d                  render_params;
	xen::Camera3dCylinder                    camera;
	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
};

StarfieldState* star_state;

void* init(xen::Kernel& kernel, const void* params){
	xen::GraphicsModuleApi* mod_graphics = (xen::GraphicsModuleApi*)xen::getModuleApi(xen::hash("graphics"));
	XenAssert(mod_graphics != nullptr, "Graphics module must be loaded before starfield module");

	StarfieldState* ss = (StarfieldState*)xen::kernelAlloc(sizeof(StarfieldState));

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
	ss->vertex_spec[1] = xen::VertexAttribute::Color4b;

	ss->window = mod_graphics->createWindow(Vec2u{800, 600}, "Starfield");

	ss->mesh_stars      = mod_graphics->createMesh(ss->vertex_spec, STAR_COUNT, ss->star_positions, ss->star_colors);
	ss->mesh_axes       = mod_graphics->createMesh(ss->vertex_spec, xen::TestMeshGeometry_Axes);
	ss->mesh_cube_lines = mod_graphics->createMesh(ss->vertex_spec, xen::TestMeshGeometry_UnitCubeLines);

	xen::clearToZero(ss->render_commands);

	ss->render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	ss->render_commands[0].color                  = xen::Color::WHITE4f;
	ss->render_commands[0].model_matrix           = xen::Scale3d(100_r);
	ss->render_commands[0].mesh                   = ss->mesh_axes;

	ss->render_commands[1].primitive_type         = xen::PrimitiveType::POINTS;
	ss->render_commands[1].color                  = xen::Color::WHITE4f;
	ss->render_commands[1].model_matrix           = Mat4r::Identity;
	ss->render_commands[1].mesh                   = ss->mesh_stars;

	ss->render_commands[2].primitive_type         = xen::PrimitiveType::LINES;
	ss->render_commands[2].color                  = xen::Color::CYAN4f;
	ss->render_commands[2].model_matrix           = (xen::Scale3d(200_r) *
	                                                 xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	                                                 );
	ss->render_commands[2].mesh                   = ss->mesh_cube_lines;

	printf("Finished starfield init\n");

	return ss;
}

void shutdown(xen::Kernel& kernel){
	xen::kernelFree(star_state);
	star_state = nullptr;
}

void tick(xen::Kernel& kernel, const xen::TickContext& cntx){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(star_state->window) };

	xen::WindowEvent* event;
	while((event = xen::pollEvent(star_state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
			gmod->destroyWindow(star_state->window);
			xen::requestKernelShutdown();
			break;
		default: break;
		}
	}

	handleCameraInputCylinder(star_state->window, star_state->camera, xen::asSeconds<real>(cntx.dt));
	star_state->render_params.camera = xen::generateCamera3d(star_state->camera);

	for(u32 i = 0; i < STAR_COUNT; ++i){
		star_state->star_positions[i].z += xen::asSeconds<real>(cntx.dt) * 100.0f;
		if(star_state->star_positions[i].z >= 100.0f){
			star_state->star_positions[i].z -= 200.0f;
		}
	}
	gmod->updateMeshVertexData(star_state->mesh_stars, 0, star_state->star_positions);

	gmod->clear      (star_state->window, xen::Color{20,20,20,255});
	gmod->render     (star_state->window, viewport,
	                                star_state->render_params, star_state->render_commands);
	gmod->swapBuffers(star_state->window);
}

void* load(xen::Kernel& kernel, void* data, const void* params){
	star_state = (StarfieldState*)data;

	return (void*)true;
}

xen::Module exported_xen_module = {
	xen::hash("game"),
	&init,
	&shutdown,
	&load,
	&tick,
};
