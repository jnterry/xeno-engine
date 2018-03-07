#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/core/time.hpp>
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

#define LINE_COUNT_SQRT (50)
#define LINE_COUNT      (LINE_COUNT_SQRT * LINE_COUNT_SQRT)

int main(int argc, char** argv){
	render_params.camera.z_near   =  0.001;
	render_params.camera.z_far    =  1000;
	render_params.camera.fov_y    =  70_deg;
	render_params.camera.look_dir = -Vec3r::UnitZ;
	render_params.camera.position =  Vec3r{0, 0, 50};

	Vec2r window_size = {800, 600};

	xen::Allocator*      alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear     arena  = xen::createArenaLinear(*alloc, xen::megabytes(32));
	xen::GraphicsDevice* device = xen::createRasterizerDevice(arena);
	xen::Window*         app    = device->createWindow((Vec2u)window_size, "starfield");

	Vec3r axis_line_verts[] = {
		Vec3r::Origin, Vec3r::UnitX,
		Vec3r::Origin, Vec3r::UnitY,
		Vec3r::Origin, Vec3r::UnitZ,
	};

	Vec3r parallel_lines[LINE_COUNT * 2];
	for(int xi = 0; xi < LINE_COUNT_SQRT; ++xi){
		real x = (real)xi / (real)LINE_COUNT_SQRT;
		for(int yi = 0; yi < LINE_COUNT_SQRT; ++yi){
		  real y = (real)yi / (real)LINE_COUNT_SQRT;

			parallel_lines[(xi*LINE_COUNT_SQRT + yi) * 2 + 0] = {x, y, 0};
			parallel_lines[(xi*LINE_COUNT_SQRT + yi) * 2 + 1] = {x, y, 1};
		}
	}

	printf("LINE COUNT: %i\n", LINE_COUNT);

	xen::FixedArray<xen::RenderCommand3d, 5> render_commands;
	render_commands[0].type                = xen::RenderCommand3d::LINES;
	render_commands[0].color               = xen::Color::RED4f;
	render_commands[0].model_matrix        = xen::Scale3d(100_r);
	render_commands[0].verticies.verticies = &axis_line_verts[0];
	render_commands[0].verticies.count     = 2;

	render_commands[1].type                = xen::RenderCommand3d::LINES;
	render_commands[1].color               = xen::Color::GREEN4f;
	render_commands[1].model_matrix        = xen::Scale3d(100_r);
	render_commands[1].verticies.verticies = &axis_line_verts[2];
	render_commands[1].verticies.count     = 2;

	render_commands[2].type                = xen::RenderCommand3d::LINES;
	render_commands[2].color               = xen::Color::BLUE4f;
	render_commands[2].model_matrix        = xen::Scale3d(100_r);
	render_commands[2].verticies.verticies = &axis_line_verts[4];
	render_commands[2].verticies.count     = 2;

	////////////////////

	render_commands[3].type                = xen::RenderCommand3d::LINES;
	render_commands[3].color               = xen::Color::YELLOW4f;
	render_commands[3].model_matrix        = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                          xen::Scale3d(50_r, 50_r, 10_r) *
	                                          xen::Rotation3dy( 30_deg)
	                                         );
	render_commands[3].verticies.verticies = &parallel_lines[0];
	render_commands[3].verticies.count     = LINE_COUNT * 2;

	render_commands[4].type                = xen::RenderCommand3d::LINES;
	render_commands[4].color               = xen::Color::MAGENTA4f;
	render_commands[4].model_matrix        = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                          xen::Scale3d(50_r, 50_r, 10_r) *
	                                          xen::Rotation3dy(-30_deg)
	                                         );
	render_commands[4].verticies.verticies = &parallel_lines[0];
	render_commands[4].verticies.count     = LINE_COUNT * 2;

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
