#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/core/time.hpp>
#include <xen/core/array.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Window.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/vertex_group_types.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include "../common.cpp"

xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	render_params.camera.z_near   =  0.001;
	render_params.camera.z_far    =  1000;
	render_params.camera.fov_y    =  70_deg;
	render_params.camera.look_dir = -Vec3r::UnitZ;
	render_params.camera.position =  Vec3r{0, 0, 10};

	Vec2r window_size = {800, 600};

	xen::Allocator*      alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear     arena  = xen::createArenaLinear(*alloc, xen::megabytes(32));
	xen::GraphicsDevice* device = xen::createRasterizerDevice(arena);
	xen::Window*         app    = device->createWindow((Vec2u)window_size, "triangle-test");

	Vec3r axis_line_verts[] = {
		Vec3r::Origin, Vec3r::UnitX,
		Vec3r::Origin, Vec3r::UnitY,
		Vec3r::Origin, Vec3r::UnitZ,
	};

	Vec3r test_triangles[] = {
		{ 0.0, 0.0, 0.0},   { 1.0, 0.0, 0.0},   { 0.0, 1.0, 0.0 },
		{ 0.0, 0.0, 5.0},   { 0.3, 0.0, 5.0},   { 0.0, 0.3, 5.0 },
		{ 0.0, 1.0, 0.0},   { 0.0, 0.3, 5.0},   { 0.0, 2.0, 0.0 },
		{ 0.0, 3.0, 5.0},   { 0.0, 1.0, 5.0},   { 0.0, 1.0, 0.0 },
	};

	xen::FixedArray<xen::RenderCommand3d, 8> render_commands;
	xen::clearToZero(render_commands);
	render_commands[0].primative_type         = xen::PrimativeType::LINES;
	render_commands[0].color                  = xen::Color::RED4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate.position     = &axis_line_verts[0];
	render_commands[0].immediate.vertex_count = 2;

	render_commands[1].primative_type         = xen::PrimativeType::LINES;
	render_commands[1].color                  = xen::Color::GREEN4f;
	render_commands[1].model_matrix           = xen::Scale3d(100_r);
	render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[1].immediate.position     = &axis_line_verts[2];
	render_commands[1].immediate.vertex_count = 2;

	render_commands[2].primative_type         = xen::PrimativeType::LINES;
	render_commands[2].color                  = xen::Color::BLUE4f;
	render_commands[2].model_matrix           = xen::Scale3d(100_r);
	render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[2].immediate.position     = &axis_line_verts[4];
	render_commands[2].immediate.vertex_count = 2;

	render_commands[3].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[3].color                  = xen::Color::YELLOW4f;
	render_commands[3].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[3].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[3].immediate.position     = &test_triangles[0];
	render_commands[3].immediate.vertex_count = 3;

	render_commands[4].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[4].color                  = xen::Color::MAGENTA4f;
	render_commands[4].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[4].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[4].immediate.position     = &test_triangles[3];
	render_commands[4].immediate.vertex_count = 3;

	render_commands[5].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[5].color                  = xen::Color::CYAN4f;
	render_commands[5].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[5].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[5].immediate.position     = &test_triangles[6];
	render_commands[5].immediate.vertex_count = 3;

	render_commands[6].primative_type         = xen::PrimativeType::LINE_STRIP;
	render_commands[6].color                  = xen::Color::WHITE4f;
	render_commands[6].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[6].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[6].immediate.position     = &test_triangles[6];
	render_commands[6].immediate.vertex_count = 3;

	render_commands[7].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[7].color                  = xen::Color::GREEN4f;
	render_commands[7].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[7].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[7].immediate.position     = &test_triangles[8];
	render_commands[7].immediate.vertex_count = 3;

	// make it stupidly big so we always render to the entire screen
	xen::Aabb2u viewport = { 0, 0, 100000, 100000 };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");
	while(xen::isWindowOpen(app)) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		printf("dt: %f\n", dt);
		xen::WindowEvent* event;
		while((event = xen::pollEvent(app)) != nullptr){
			switch(event->type){
			case xen::WindowEvent::Closed:
				device->destroyWindow(app);
				break;
			default: break;
			}
		}
		handleCameraInputPlane(render_params.camera, dt);

		// Rendering
		device->clear(app, xen::Color::BLACK);
		device->render(app, viewport, render_params, render_commands);
		device->swapBuffers(app);
	}
	printf("Exiting main loop\n");

	xen::destroyArenaLinear(*alloc, arena);
	delete alloc;

	return 0;
}
