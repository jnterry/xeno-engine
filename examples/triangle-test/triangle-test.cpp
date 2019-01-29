#include <stdio.h>

#include <xen/graphics/TestMeshes.hpp>
#include "../utilities.hpp"

Vec3r test_triangles_pos[] = {
	{ 0.0_r, 0.0_r, 0.0_r},   { 1.0_r, 0.0_r, 0.0_r},   { 0.0_r, 1.0_r, 0.0_r },
	{ 0.0_r, 0.0_r, 5.0_r},   { 0.3_r, 0.0_r, 5.0_r},   { 0.0_r, 0.3_r, 5.0_r },
	{ 0.0_r, 0.0_r, 0.0_r},   { 0.0_r, 0.3_r, 5.0_r},   { 0.0_r, 1.0_r, 0.0_r },
	{-1.0_r, 3.0_r, 5.0_r},   {-1.0_r, 0.0_r, 3.0_r},   {-1.0_r, 1.0_r, 0.0_r },
};

xen::Color test_triangles_color[] = {
	xen::Color::WHITE, xen::Color::RED, xen::Color::GREEN,
	{100, 255, 255, 255}, {255, 100, 255, 255}, {255, 255, 100, 255},
	xen::Color::YELLOW, xen::Color::RED, xen::Color::YELLOW,
	xen::Color::MAGENTA, xen::Color::YELLOW, xen::Color::CYAN,
};

struct State {
	xen::RenderParameters3d render_params;
	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;

	xen::Window*      window;
	xen::RenderTarget window_target;

	xen::Mesh mesh_triangles;
	xen::Mesh mesh_axes;
	xen::Mesh mesh_cube;

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
};

State* state = nullptr;

void* init(const void* params){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();
	XenAssert(mod_ren != nullptr, "Graphics module must be loaded before cornell-box");
	XenAssert(mod_win != nullptr, "Window module must be loaded before cornell-box");

	state = (State*)xen::kernelAlloc(sizeof(State));
	xen::clearToZero(state);

	state->window        = mod_win->createWindow({800, 600}, "triangle-test");
	state->window_target = mod_ren->createWindowRenderTarget(state->window);

	state->render_params.lights.size = 0;
	state->render_params.ambient_light = {1,1,1};
	state->render_params.camera.z_near   =  0.001;
	state->render_params.camera.z_far    =  1000;
	state->render_params.camera.fov_y    =  70_deg;
	state->render_params.camera.up_dir   =  Vec3r::UnitY;
	state->render_params.camera.look_dir = -Vec3r::UnitZ;
	state->render_params.camera.position =  Vec3r{0, 0, 10};

	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;

	state->mesh_triangles = mod_ren->createMesh
		(state->vertex_spec, XenArrayLength(test_triangles_pos),
		 test_triangles_pos, nullptr, test_triangles_color
		);
	state->mesh_axes = mod_ren->createMesh(state->vertex_spec, xen::TestMeshGeometry_Axes);
	state->mesh_cube = mod_ren->createMesh(state->vertex_spec, xen::TestMeshGeometry_UnitCube);

	state->render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	state->render_commands[0].color                  = xen::Color::WHITE4f;
	state->render_commands[0].model_matrix           = xen::Scale3d(100_r);
	state->render_commands[0].mesh                   = state->mesh_axes;

	state->render_commands[1].primitive_type         = xen::PrimitiveType::TRIANGLES;
	state->render_commands[1].color                  = xen::Color::WHITE4f;
	state->render_commands[1].model_matrix           = xen::Scale3d(1, 1, 1);
	state->render_commands[1].mesh                   = state->mesh_triangles;

	state->render_commands[2].primitive_type         = xen::PrimitiveType::TRIANGLES;
	state->render_commands[2].color                  = xen::Color::WHITE4f;
	state->render_commands[2].model_matrix           = xen::Scale3d(1, 1, 1);
	state->render_commands[2].mesh                   = state->mesh_cube;

	return state;
}

void* load(void* data, const void* params){
  state = (State*)data;
	return (void*)true;
}

void tick(const xen::TickContext& cntx){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();

	xen::Aabb2u viewport = { Vec2u::Origin, mod_win->getClientAreaSize(state->window) };

	xen::WindowEvent* event;
	while((event = mod_win->pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
			mod_win->destroyWindow(state->window);
			xen::requestKernelShutdown();
			break;
		default: break;
		}
	}
	handleCameraInputPlane(mod_win, state->window, state->render_params.camera, xen::asSeconds<real>(cntx.dt));

	state->render_commands[2].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                          xen::Rotation3dy(90_deg * xen::asSeconds<real>(cntx.time)) *
	                                          xen::Translation3d(0_r, 3_r, 0_r)
	                                         );

	// Rendering
	mod_ren->clear      (state->window_target, xen::Color{20,20,20,255});
	mod_ren->render     (state->window_target, viewport, state->render_params, state->render_commands);
	mod_ren->swapBuffers(state->window_target);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
