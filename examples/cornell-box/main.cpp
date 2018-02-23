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
real camera_speed = 250;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

void handleInput(real dt){
	SDL_PumpEvents();

	const u8* keystate = SDL_GetKeyboardState(NULL);

	if(keystate[SDL_SCANCODE_UP]){
		camera.radius -= camera_speed * dt;
	}
	if(keystate[SDL_SCANCODE_DOWN]){
		camera.radius += camera_speed * dt;
	}
	camera.radius = xen::clamp(camera.radius, 0.01_r, 750_r);
	//camera_x.position = (camera.radius + 50_r) * Vec3r::UnitX;
	//camera_y.position = (camera.radius + 50_r) * Vec3r::UnitY;
	//camera_z.position = (camera.radius + 50_r) * Vec3r::UnitZ;

	if(keystate[SDL_SCANCODE_LEFT]){
		camera.angle -= camera_rotate_speed * dt;
	}
	if(keystate[SDL_SCANCODE_RIGHT]){
		camera.angle += camera_rotate_speed * dt;
	}
	if(keystate[SDL_SCANCODE_A]){
		camera.height += camera_speed * dt;
	}
	if(keystate[SDL_SCANCODE_Z]){
		camera.height -= camera_speed * dt;
	}

	if(keystate[SDL_SCANCODE_Q]){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
	if(keystate[SDL_SCANCODE_E]){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = {1.0_r, 1.0_r, 1.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f.rgb;
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

	xen::FixedArray<xen::RenderCommand3d, 1> render_commands;
	render_commands[0].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[0].color               = xen::Color::RED4f;
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
		handleInput(dt);

		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);

		// Do rendering
		render_params.camera = xen::generateCamera3d(camera);
		xen::sren::renderRaytrace(screen->buffer, viewport,
		                           render_params, render_commands);

		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	return 0;
}
