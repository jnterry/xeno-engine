#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/sren/renderer3d.hxx>

#include <SDL.h>
#include "../SDLauxilary.h"

#include "testModel.hpp"

xen::Camera3dCylinder camera;
xen::Camera3d         camera_x;
xen::Camera3d         camera_y;
xen::Camera3d         camera_z;

static const real       Z_NEAR = 0.001_r;
static const real       Z_FAR  = 1000_r;
static const xen::Angle FOV_Y  = 70_deg;

int main(int argc, char** argv){
	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 450;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	camera.angle    = 0.0_deg;

	Vec2r window_size = {800, 800};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

	xen::RenderCommand3d render_commands[1];
	render_commands[0].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[0].color               = xen::Color::RED;
	render_commands[0].model_matrix        = xen::Translation3d(-277_r, -277_r, -277_r);
	render_commands[0].verticies.verticies = &test_model_geometry[0];
	render_commands[0].verticies.count     = test_model_num_vertices;

	int last_tick = SDL_GetTicks();

	// make it stupidly big so we always render to the entire screen
	xen::Aabb2u viewport = { 0, 0, 100000, 100000 };

	printf("Entering main loop\n");
	while(NoQuitMessageSDL()) {
		int tick = SDL_GetTicks();
		float dt = ((float)(tick - last_tick)) / 1000.0f;
		last_tick = tick;

		printf("dt: %f\n", dt);
		handleCameraInput(camera, dt);

		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);

		// Do rendering
		xen::sren::renderRasterize(screen->buffer, viewport,
															 xen::generateCamera3d(camera),
															 render_commands, XenArrayLength(render_commands)
															);

		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	return 0;
}
