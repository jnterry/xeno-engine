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
#include "SDLauxilary.h"

xen::Camera3dOrbit camera;
xen::Camera3d      camera_x;
xen::Camera3d      camera_y;
xen::Camera3d      camera_z;
real camera_speed = 250;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

const u32 STAR_COUNT = 0; //1024;

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
	camera.radius = xen::clamp(camera.radius, 0.01_r, 750_r);

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

static const real       Z_NEAR = 0.001_r;
static const real       Z_FAR  = 1000_r;
static const xen::Angle FOV_Y  = 70_deg;

int main(int argc, char** argv){
	camera_x.z_near   = Z_NEAR;
	camera_x.z_far    = Z_FAR;
	camera_x.fov_y    = FOV_Y;
	camera_x.position = {300, 0, 0};
	camera_x.look_dir = -Vec3r::UnitX;
	camera_x.up_dir   =  Vec3r::UnitY;

	camera_y.z_near   = Z_NEAR;
	camera_y.z_far    = Z_FAR;
	camera_y.fov_y    = FOV_Y;
	camera_y.position = {0, 300, 0};
	camera_y.look_dir = -Vec3r::UnitY;
	camera_y.up_dir   =  Vec3r::UnitX;

	camera_z.z_near   = Z_NEAR;
	camera_z.z_far    = Z_FAR;
	camera_z.fov_y    = FOV_Y;
	camera_z.position = {0, 0, 300};
	camera_z.look_dir = -Vec3r::UnitZ;
	camera_z.up_dir   =  Vec3r::UnitY;

	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 450;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	//:TODO: breaks if angle is exactly 0deg, never occurs
	// under user control since don't hit dead on float value, but
	// broken if set here
	camera.angle    = 0.0_deg;

	Vec2r window_size = {800, 600};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

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

	Vec3r mesh_verts[] = {
		Vec3r{ 0_r, 0_r, 0_r },
		Vec3r{ 1_r, 0_r, 0_r },
		Vec3r{ 0_r, 1_r, 0_r },

		Vec3r{ 0_r, 0_r, 0_r },
		Vec3r{ 1_r, 0_r, 0_r },
		Vec3r{ 0_r, 0_r, 1_r },
	};

	xen::RenderCommand3d render_commands[7];
	render_commands[0].type                = xen::RenderCommand3d::LINES;
	render_commands[0].color               = xen::Color::RED;
	render_commands[0].model_matrix        = xen::Scale3d(100_r);
	render_commands[0].verticies.verticies = &axis_line_verts[0];
	render_commands[0].verticies.count     = 2;

	render_commands[1].type                = xen::RenderCommand3d::LINES;
	render_commands[1].color               = xen::Color::GREEN;
	render_commands[1].model_matrix        = xen::Scale3d(100_r);
	render_commands[1].verticies.verticies = &axis_line_verts[2];
	render_commands[1].verticies.count     = 2;

	render_commands[2].type                = xen::RenderCommand3d::LINES;
	render_commands[2].color               = xen::Color::BLUE;
	render_commands[2].model_matrix        = xen::Scale3d(100_r);
	render_commands[2].verticies.verticies = &axis_line_verts[4];
	render_commands[2].verticies.count     = 2;

	render_commands[3].type                = xen::RenderCommand3d::POINTS;
	render_commands[3].color               = xen::Color::WHITE;
	render_commands[3].model_matrix        = Mat4r::Identity;
	render_commands[3].verticies.verticies = star_positions;
	render_commands[3].verticies.count     = STAR_COUNT;

	render_commands[4].type                = xen::RenderCommand3d::LINES;
	render_commands[4].color               = 0xFFFFFF00;
	render_commands[4].model_matrix        = xen::Scale3d(200_r) * xen::Translation3d(-100.0_r, -100.0_r, -100.0_r);
	render_commands[4].verticies.verticies = &cube_lines[0];
	render_commands[4].verticies.count     = XenArrayLength(cube_lines);

	render_commands[5].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[5].color               = 0x00FF00FF;
	render_commands[5].model_matrix        = xen::Scale3d(50_r)* xen::Rotation3dy(90_deg); // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r);
	render_commands[5].verticies.verticies = &mesh_verts[0];
	render_commands[5].verticies.count     = 3;

	render_commands[6].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[6].color               = 0x00FFFF00;
	render_commands[6].model_matrix        = xen::Scale3d(50_r)* xen::Rotation3dy(90_deg); // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r);
	render_commands[6].verticies.verticies = &mesh_verts[3];
	render_commands[6].verticies.count     = 3;

	int last_tick = SDL_GetTicks();

	u32 raytrace_size = 128;

	// make it stupidly big so we always render to the entire screen
	//xen::Aabb2u viewport = { 0, 0, 100000, 100000 };
	u32 viewport_padding = 10;
	Vec2u window_size_u = (Vec2u)window_size;
	xen::Aabb2u viewport_main = xen::makeAabbFromMinAndSize
		(
		 viewport_padding,
		 (window_size_u.y - raytrace_size) / 2,
		 raytrace_size,
		 raytrace_size
		);

	Vec2u preview_size = { (window_size_u.x - viewport_padding * 4 - raytrace_size) / 2,
	                       (window_size_u.y - viewport_padding * 3                ) / 2 };

	xen::Aabb2u viewport_y = xen::makeAabbFromMinAndSize
		(
		 viewport_padding * 2 + raytrace_size,
		 viewport_padding,
		 preview_size.x, preview_size.y
		);

	xen::Aabb2u viewport_x = xen::makeAabbFromMinAndSize
		(
		 viewport_padding * 2 + raytrace_size,
		 viewport_padding * 2 + preview_size.y,
		 preview_size.x, preview_size.y
		);

	xen::Aabb2u viewport_z = xen::makeAabbFromMinAndSize
		(
		 viewport_padding * 3 + raytrace_size + preview_size.x,
		 viewport_padding * 2 + preview_size.y,
		 preview_size.x, preview_size.y
		);


	printf("Entering main loop\n");
	while(NoQuitMessageSDL()) {
		int tick = SDL_GetTicks();
		float dt = ((float)(tick - last_tick)) / 1000.0f;
		last_tick = tick;

		printf("dt: %f\n", dt);
		handleInput(dt);

		for(u32 i = 0; i < STAR_COUNT; ++i){
			star_positions[i].z += dt * 75.0f;
			if(star_positions[i].z >= 100.0f){
				star_positions[i].z -= 200.0f;
			}
		}

		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);


		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);
		xen::sren::clear(screen->buffer, xen::Color::WHITE);

		// Do rendering
		xen::sren::clear(screen->buffer, viewport_main, xen::Color::BLACK);
		xen::sren::renderRasterize(screen->buffer, viewport_main,
		                           xen::generateCamera3d(camera),
		                           render_commands, XenArrayLength(render_commands)
		                           );
		xen::sren::renderRaytrace(screen->buffer, viewport_main,
		                          xen::generateCamera3d(camera),
		                          render_commands, XenArrayLength(render_commands)
		                         );

		xen::sren::clear(screen->buffer, viewport_x, xen::Color::BLACK);
		xen::sren::renderRasterize(screen->buffer, viewport_x,
		                           camera_x,
		                           render_commands, XenArrayLength(render_commands)
		                          );
		xen::sren::clear(screen->buffer, viewport_y, xen::Color::BLACK);
		xen::sren::renderRasterize(screen->buffer, viewport_y,
		                           camera_y,
		                           render_commands, XenArrayLength(render_commands)
		                          );
		xen::sren::clear(screen->buffer, viewport_z, xen::Color::BLACK);
		xen::sren::renderRasterize(screen->buffer, viewport_z,
		                           camera_z,
		                           render_commands, XenArrayLength(render_commands)
		                           );


		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	return 0;
}
