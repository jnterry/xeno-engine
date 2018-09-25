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

	xen::Window* window;

	xen::Mesh mesh_triangles;
	xen::Mesh mesh_axes;
	xen::Mesh mesh_cube;

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
};

State* state = nullptr;

void* init(xen::Kernel& kernel, const void* params){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi(xen::hash("graphics"));
	XenAssert(gmod != nullptr, "A graphics module must be loaded before triangle-test");

	state = (State*)xen::kernelAlloc(sizeof(State));
	xen::clearToZero(state);

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

	state->window = gmod->createWindow({800, 600}, "triangle-test");

	state->mesh_triangles = gmod->createMesh
		(state->vertex_spec, XenArrayLength(test_triangles_pos),
		 test_triangles_pos, nullptr, test_triangles_color
		);
	state->mesh_axes = gmod->createMesh(state->vertex_spec, xen::TestMeshGeometry_Axes);
	state->mesh_cube = gmod->createMesh(state->vertex_spec, xen::TestMeshGeometry_UnitCube);

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

void* load(xen::Kernel& kernel, void* data, const void* params){
  state = (State*)data;
	return (void*)true;
}

void tick(xen::Kernel& kernel, const xen::TickContext& cntx){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi(xen::hash("graphics"));
	XenAssert(gmod != nullptr, "A graphics module must be loaded before triangle-test");

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(state->window) };

	xen::WindowEvent* event;
	while((event = xen::pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
			gmod->destroyWindow(state->window);
			xen::requestKernelShutdown();
				break;
		default: break;
		}
	}
	handleCameraInputPlane(state->window, state->render_params.camera, xen::asSeconds<real>(cntx.dt));

	state->render_commands[2].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                          xen::Rotation3dy(90_deg * xen::asSeconds<real>(cntx.time)) *
	                                          xen::Translation3d(0_r, 3_r, 0_r)
	                                         );

	// Rendering
	gmod->clear      (state->window, xen::Color{20,20,20,255});
	gmod->render     (state->window, viewport, state->render_params, state->render_commands);
	gmod->swapBuffers(state->window);
}

void shutdown(xen::Kernel& kernel){
	xen::kernelFree(state);
}

xen::Module exported_xen_module = {
	xen::hash("game"),
	&init,
	&shutdown,
	&load,
	&tick,
};
