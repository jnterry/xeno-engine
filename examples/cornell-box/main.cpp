#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/core/time.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include <SDL.h>
#include "../SDLauxilary.h"

#include "testModel.hpp"

xen::Camera3dCylinder camera;

xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = {1.0_r, 1.0_r, 1.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f;
	scene_lights[0].attenuation    = {0.0f, 0.0f, 1.0f};

	render_params.ambient_light = xen::Color3f(0.3f, 0.3f, 0.3f);
	render_params.lights        = scene_lights;

	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 450;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	camera.angle    = 0.0_deg;

	Vec2r window_size = {300, 300};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

	xen::Allocator*      alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear     arena  = xen::createArenaLinear(*alloc, xen::megabytes(32));
	xen::GraphicsDevice* device = xen::createRasterizerDevice(arena, screen->buffer);

	xen::FixedArray<xen::RenderCommand3d, 1> render_commands;
	render_commands[0].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[0].color               = xen::Color::RED4f;
	render_commands[0].model_matrix        = xen::Translation3d(-277_r, -277_r, -277_r);
	render_commands[0].verticies.verticies = &test_model_geometry[0];
	render_commands[0].verticies.count     = test_model_num_vertices;

	int last_tick = SDL_GetTicks();

	xen::Aabb2u viewport = { 0, 0, (u32)window_size.x, (u32)window_size.y };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");
	while(NoQuitMessageSDL()) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		printf("dt: %f\n", dt);
		handleCameraInput(camera, dt);

		// Clear buffer
		device->clear(xen::makeNullHandle<xen::RenderTarget>(), viewport, xen::Color::BLACK);

		// Do rendering
		render_params.camera = xen::generateCamera3d(camera);
	  device->render(xen::makeNullHandle<xen::RenderTarget>(), viewport, render_params, render_commands);

		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	xen::destroyArenaLinear(*alloc, arena);
	delete alloc;

	return 0;
}
