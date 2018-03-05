#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/vertex_group_types.hpp>
#include <xen/sren/renderer3d.hxx>

#include <SDL.h>
#include "../SDLauxilary.h"

void handleCameraInput(xen::Camera3d& camera, real dt) {
	const static constexpr real camera_speed = 10.0f;
	SDL_PumpEvents();
	const u8* keystate = SDL_GetKeyboardState(NULL);
	if(keystate[SDL_SCANCODE_UP]){
		camera.position.y += camera_speed * dt;
	}
	if(keystate[SDL_SCANCODE_DOWN]){
	  camera.position.y -= camera_speed * dt;
	}
	if(keystate[SDL_SCANCODE_LEFT]){
		camera.position.x += camera_speed * dt;
	}
	if(keystate[SDL_SCANCODE_RIGHT]){
		camera.position.x -= camera_speed * dt;
	}
}

xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	render_params.camera.z_near   =  0.001;
	render_params.camera.z_far    =  1000;
	render_params.camera.fov_y    =  70_deg;
	render_params.camera.look_dir = -Vec3r::UnitZ;
	render_params.camera.position =  Vec3r{0, 0, 50};

	Vec2r window_size = {800, 600};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

	Vec3r axis_line_verts[] = {
		Vec3r::Origin, Vec3r::UnitX,
		Vec3r::Origin, Vec3r::UnitY,
		Vec3r::Origin, Vec3r::UnitZ,
	};

	xen::Triangle3r test_triangle;
	test_triangle.p1 = Vec3r{0, 0, 0};
	test_triangle.p2 = Vec3r{0, 1, 0};
	test_triangle.p3 = Vec3r{1, 0, 0};

	xen::FixedArray<xen::RenderCommand3d, 4> render_commands;
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

	render_commands[3].type                = xen::RenderCommand3d::TRIANGLES;
	render_commands[3].color               = xen::Color::YELLOW4f;
	render_commands[3].model_matrix        = xen::Scale3d(50, 50, 50);
	render_commands[3].verticies.verticies = &test_triangle.vertices[0];
	render_commands[3].verticies.count     = 3;

	int last_tick = SDL_GetTicks();

	// make it stupidly big so we always render to the entire screen
	xen::Aabb2u viewport = { 0, 0, 100000, 100000 };

	printf("Entering main loop\n");
	while(NoQuitMessageSDL()) {
		int tick = SDL_GetTicks();
	  real dt = ((float)(tick - last_tick)) / 1000.0f;
		last_tick = tick;

		printf("dt: %f\n", dt);
		handleCameraInput(render_params.camera, dt);

		// Clear buffer
		xen::sren::clear(screen->buffer, xen::Color::BLACK);
		xen::sren::renderRasterize(screen->buffer, viewport, render_params, render_commands);
		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	KillSDL(screen);

	return 0;
}
