#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Angle.hpp>
#include <xen/math/Quaternion.hpp>
#include <xen/sren/renderer3d.hxx>

#include <SDL.h>
#include "SDLauxilary.h"

xen::Camera3dOrbit camera;
real camera_speed = 100;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

const u32 STAR_COUNT = 1024;

Vec3r star_positions[STAR_COUNT];

void handleInput(real dt){
	SDL_PumpEvents();

	const u8* keystate = SDL_GetKeyboardState(NULL);

	if(keystate[SDL_SCANCODE_UP]){
		camera.radius -= camera_speed * dt;
	}
	if(keystate[SDL_SCANCODE_DOWN]){
		camera.radius += camera_speed * dt;
	}
	camera.radius = xen::clamp(camera.radius, 0.01_r, 250_r);

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
}

int main(int argc, char** argv){
	camera.z_near   = 0.1;
	camera.z_far    = 100;
	camera.fov_y    = 1_deg;
	camera.radius   = 100;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	//:TODO: breaks if angle is exactly +90deg, never occurs
	// under user control since don't hit dead on float value, but
	// broken if set here
	camera.angle    = -90.0_deg;

	Vec2r window_size = {800, 600};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

	int grid_count = xen::sqrt(STAR_COUNT);
	for(u32 i = 0; i < STAR_COUNT; ++i){
		//int grid_x = i % grid_count;
		//int grid_y = i / grid_count;
		//star_positions[i].x = xen::lerp(-100, 100, (float)grid_x / (float)grid_count);
		//star_positions[i].y = xen::lerp(-100, 100, (float)grid_y / (float)grid_count);
		//star_positions[i].z = -100;

		star_positions[i].x = xen::randf(-100, 100);
		star_positions[i].y = xen::randf(-100, 100);
		star_positions[i].z = xen::randf(-100, 100);
	}

	Vec3r axis_line_verts[] = {
		{   0.0_r,   0.0_r,     0.0_r },
		{ 100.0_r,   0.0_r,     0.0_r },

		{   0.0_r,   0.0_r,     0.0_r },
		{   0.0_r, 100.0_r,     0.0_r },

		{   0.0_r,   0.0_r,     0.0_r },
		{   0.0_r,   0.0_r,   100.0_r },
	};

	xen::RenderCommand3d render_commands[4];
	render_commands[0].type                = xen::RenderCommand3d::POINTS;
	render_commands[0].color               = xen::Color::WHITE;
	render_commands[0].verticies.verticies = star_positions;
	render_commands[0].verticies.count     = STAR_COUNT;

	render_commands[1].type                = xen::RenderCommand3d::LINE_STRIP;
	render_commands[1].color               = xen::Color::BLUE;
	render_commands[1].verticies.verticies = &axis_line_verts[0];
	render_commands[1].verticies.count     = 2;

	render_commands[2].type                = xen::RenderCommand3d::LINE_STRIP;
	render_commands[2].color               = xen::Color::GREEN;
	render_commands[2].verticies.verticies = &axis_line_verts[2];
	render_commands[2].verticies.count     = 2;

	render_commands[3].type                = xen::RenderCommand3d::LINE_STRIP;
	render_commands[3].color               = xen::Color::RED;
	render_commands[3].verticies.verticies = &axis_line_verts[4];
	render_commands[3].verticies.count     = 2;

	int last_tick = SDL_GetTicks();

	printf("Entering main loop\n");
	while(NoQuitMessageSDL()) {
		int tick = SDL_GetTicks();
		float dt = ((float)(tick - last_tick)) / 1000.0f;
		last_tick = tick;

		printf("dt: %f\n", dt);
		handleInput(dt);

		for(u32 i = 0; i < STAR_COUNT; ++i){
			star_positions[i].z += dt * 50.0f;
			if(star_positions[i].z >= 100.0f){
				star_positions[i].z = -100.0f;
			}
		}

		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);

		//camera.angle += dt * 30_deg;
		Mat4f mat_vp = xen::getViewProjectionMatrix(camera, window_size);

		Vec3f screen_space;
		xen::Color color = {255, 255, 255, 0};

		xen::sren::renderRasterize(screen->buffer, xen::generateCamera3d(camera), render_commands, 4);

		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	return 0;
}
