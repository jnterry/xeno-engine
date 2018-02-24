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

xen::Camera3dCylinder camera;
xen::Camera3d         camera_x;
xen::Camera3d         camera_y;
xen::Camera3d         camera_z;

static const real       Z_NEAR = 0.001_r;
static const real       Z_FAR  = 1000_r;
static const xen::Angle FOV_Y  = 70_deg;

xen::RenderParameters3d render_params;


int main(int argc, char** argv){
	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = {10.0_r, 0.2_r, -10.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f.rgb;
	scene_lights[0].attenuation    = {0.0f, 0.0f, 0.01f};

	render_params.ambient_light = xen::Color3f(0.3f, 0.3f, 0.3f);
	render_params.lights        = scene_lights;

	camera_x.z_near   = Z_NEAR;
	camera_x.z_far    = Z_FAR;
	camera_x.fov_y    = FOV_Y;
	camera_x.position = {300, 0, 0};
	camera_x.look_dir = Vec3r::UnitX; // :TODO: why are these look dirs not -ve of position?
	camera_x.up_dir   = Vec3r::UnitY;

	camera_y.z_near   = Z_NEAR;
	camera_y.z_far    = Z_FAR;
	camera_y.fov_y    = FOV_Y;
	camera_y.position = {0, 300, 0};
	camera_y.look_dir = Vec3r::UnitY; // :TODO: why are these look dirs not -ve of position?
	camera_y.up_dir   = Vec3r::UnitX;

	camera_z.z_near   = Z_NEAR;
	camera_z.z_far    = Z_FAR;
	camera_z.fov_y    = FOV_Y;
	camera_z.position = {0, 0, 300};
	camera_z.look_dir = Vec3r::UnitZ; // :TODO: why are these look dirs not -ve of position?
	camera_z.up_dir   = Vec3r::UnitY;

	camera.z_near = 0.001;
	camera.z_far  = 1000;
	camera.fov_y  = 70_deg;
	camera.radius = 450;
	camera.height = 0;
	camera.up_dir = Vec3r::UnitY;
	camera.axis   = Vec3r::UnitY;
	camera.target = Vec3r::Origin;
	camera.angle  = 0.0_deg;

	Vec2r window_size = {800, 800};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

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

	xen::FixedArray<xen::RenderCommand3d, 6> render_commands;
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

	render_commands[3].type                = xen::RenderCommand3d::LINES;
	render_commands[3].color               = xen::Color::CYAN4f;
	render_commands[3].model_matrix        = xen::Scale3d(200_r) * xen::Translation3d(-100.0_r, -100.0_r, -100.0_r);
	render_commands[3].verticies.verticies = &cube_lines[0];
	render_commands[3].verticies.count     = XenArrayLength(cube_lines);

	render_commands[4].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[4].color               = xen::Color::MAGENTA4f;
	render_commands[4].model_matrix        = xen::Scale3d(50_r)* xen::Rotation3dy(90_deg); // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r);
	render_commands[4].verticies.verticies = &mesh_verts[0];
	render_commands[4].verticies.count     = 3;

	render_commands[5].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[5].color               = xen::Color::YELLOW4f;
	render_commands[5].model_matrix        = xen::Scale3d(50_r)* xen::Rotation3dy(90_deg); // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r);
	render_commands[5].verticies.verticies = &mesh_verts[3];
	render_commands[5].verticies.count     = 3;

	int last_tick = SDL_GetTicks();

	u32 raytrace_size = 128;

	Vec2u window_size_u   = (Vec2u)window_size;
	u32 viewport_padding  = 10;
	Vec2u viewport_size   = { (window_size_u.x - viewport_padding * 3 ) / 2,
	                          (window_size_u.y - viewport_padding * 3 ) / 2 };


	xen::Aabb2u viewport_main = xen::makeAabbFromMinAndSize
		(
		 viewport_padding + (viewport_size.x - raytrace_size)/2,
		 viewport_padding + (viewport_size.y - raytrace_size)/2,
		 raytrace_size, raytrace_size
		);

	xen::Aabb2u viewport_y = xen::makeAabbFromMinAndSize
		(
		 viewport_padding * 2 + viewport_size.x,
		 viewport_padding,
		 viewport_size.x, viewport_size.y
		);

	xen::Aabb2u viewport_x = xen::makeAabbFromMinAndSize
		(
		 viewport_padding * 1 + 0,
		 viewport_padding * 2 + viewport_size.y,
		 viewport_size.x, viewport_size.y
		);

	xen::Aabb2u viewport_z = xen::makeAabbFromMinAndSize
		(
		 viewport_padding * 2 + viewport_size.x,
		 viewport_padding * 2 + viewport_size.y,
		 viewport_size.x, viewport_size.y
		);


	printf("Entering main loop\n");
	float run_time = 0.0f;
	while(NoQuitMessageSDL()) {
		int tick = SDL_GetTicks();
		float dt = ((float)(tick - last_tick)) / 1000.0f;
		last_tick = tick;

		run_time += dt;

		scene_lights[0].point.position.y = xen::mapToRange(-1.f, 1.f, 0.01f, 20.0f, xen::sin(run_time * 90_deg));

		printf("dt: %f\n", dt);
		handleCameraInput(camera, dt);

		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);


		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);
		xen::sren::clear(screen->buffer, xen::Color::WHITE);

		// Do rendering
		xen::sren::clear(screen->buffer, viewport_main, xen::Color::BLACK);
		render_params.camera = xen::generateCamera3d(camera);
		xen::sren::renderRasterize(screen->buffer, viewport_main,
		                           render_params, render_commands);
		xen::sren::renderRaytrace(screen->buffer, viewport_main,
		                          render_params, render_commands);

		xen::sren::clear(screen->buffer, viewport_x, xen::Color::BLACK);
		render_params.camera = xen::generateCamera3d(camera_x);
		xen::sren::renderRasterize(screen->buffer, viewport_x,
		                           render_params, render_commands);
		xen::sren::renderCameraDebug(screen->buffer, viewport_x, camera_x, xen::generateCamera3d(camera));

		xen::sren::clear(screen->buffer, viewport_y, xen::Color::BLACK);
		render_params.camera = xen::generateCamera3d(camera_y);
		xen::sren::renderRasterize(screen->buffer, viewport_y,
		                           render_params, render_commands);
		xen::sren::renderCameraDebug(screen->buffer, viewport_y, camera_y, xen::generateCamera3d(camera));

		xen::sren::clear(screen->buffer, viewport_z, xen::Color::BLACK);
		render_params.camera = xen::generateCamera3d(camera_z);
		xen::sren::renderRasterize(screen->buffer, viewport_z,
		                           render_params, render_commands);
		xen::sren::renderCameraDebug(screen->buffer, viewport_z, camera_z, xen::generateCamera3d(camera));


		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	return 0;
}
