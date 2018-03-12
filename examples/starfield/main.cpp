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
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include "../common.cpp"

xen::RenderParameters3d render_params;
xen::Camera3dCylinder camera;

const u32 STAR_COUNT = 1024;
Vec3r star_positions[STAR_COUNT];

int main(int argc, char** argv){
	camera.z_near = 0.001;
	camera.z_far  = 1000;
	camera.fov_y  = 70_deg;
	camera.radius = 450;
	camera.height = 0;
	camera.up_dir = Vec3r::UnitY;
	camera.axis   = Vec3r::UnitY;
	camera.target = Vec3r::Origin;
	camera.angle  = 0.0_deg;

	Vec2r window_size = {800, 600};

	xen::Allocator*      alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear     arena  = xen::createArenaLinear(*alloc, xen::megabytes(32));
	xen::GraphicsDevice* device = xen::createRasterizerDevice(arena);
	xen::Window*         app    = device->createWindow((Vec2u)window_size, "starfield");

	for(u32 i = 0; i < STAR_COUNT; ++i){
		star_positions[i].x = xen::randf(-100, 100);
		star_positions[i].y = xen::randf(-100, 100);
		star_positions[i].z = xen::randf(-100, 100);
	}

	Vec3r axis_line_verts[] = {
		Vec3r::Origin, Vec3r::UnitX,
		Vec3r::Origin, Vec3r::UnitY,
		Vec3r::Origin, Vec3r::UnitZ,
	};

	Vec3r cube_lines[] = {
		{ 0_r,  0_r,  0_r },
		{ 1_r,  0_r,  0_r },

		{ 1_r,  0_r,  0_r },
		{ 1_r,  1_r,  0_r },

		{ 1_r,  1_r,  0_r },
		{ 0_r,  1_r,  0_r },

		{ 0_r,  1_r,  0_r },
		{ 0_r,  0_r,  0_r },

		{ 0_r,  0_r,  1_r },
		{ 1_r,  0_r,  1_r },

		{ 1_r,  0_r,  1_r },
		{ 1_r,  1_r,  1_r },

		{ 1_r,  1_r,  1_r },
		{ 0_r,  1_r,  1_r },

		{ 0_r,  1_r,  1_r },
		{ 0_r,  0_r,  1_r },

		{ 1_r,  1_r,  1_r },
		{ 1_r,  1_r,  0_r },

		{ 0_r,  1_r,  1_r },
		{ 0_r,  1_r,  0_r },

		{ 0_r,  0_r,  1_r },
		{ 0_r,  0_r,  0_r },

		{ 1_r,  0_r,  1_r },
		{ 1_r,  0_r,  0_r },
	};

	xen::FixedArray<xen::RenderCommand3d, 5> render_commands;
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

	render_commands[3].primative_type         = xen::PrimativeType::POINTS;
	render_commands[3].color                  = xen::Color::WHITE4f;
	render_commands[3].model_matrix           = Mat4r::Identity;
	render_commands[3].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[3].immediate.position     = star_positions;
	render_commands[3].immediate.vertex_count = STAR_COUNT;

	render_commands[4].primative_type         = xen::PrimativeType::LINES;
	render_commands[4].color                  = xen::Color::CYAN4f;
	render_commands[4].model_matrix           = (xen::Scale3d(200_r) *
	                                             xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	                                            );
	render_commands[4].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[4].immediate.position     = &cube_lines[0];
	render_commands[4].immediate.vertex_count = XenArrayLength(cube_lines);

	xen::Aabb2u viewport = { 0, 0, (u32)window_size.x, (u32)window_size.y };

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
		handleCameraInputCylinder(camera, dt);
		render_params.camera = xen::generateCamera3d(camera);

		for(u32 i = 0; i < STAR_COUNT; ++i){
			star_positions[i].z += dt * 75.0f;
			if(star_positions[i].z >= 100.0f){
				star_positions[i].z -= 200.0f;
			}
		}

	  device->clear(app, xen::Color::BLACK);
		device->render(app, viewport, render_params, render_commands);
		device->swapBuffers(app);
	}
	printf("Exiting main loop\n");

	xen::destroyArenaLinear(*alloc, arena);
	delete alloc;

	return 0;
}
