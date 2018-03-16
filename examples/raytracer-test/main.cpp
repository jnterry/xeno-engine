#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/core/time.hpp>
#include <xen/core/array.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Window.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include "../common.cpp"

xen::Camera3dCylinder camera;

static const real       Z_NEAR = 0.001_r;
static const real       Z_FAR  = 1000_r;
static const xen::Angle FOV_Y  = 70_deg;

xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = {10.0_r, 0.2_r, -10.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f;
	scene_lights[0].attenuation    = {0.0f, 0.0f, 0.01f};

	render_params.ambient_light = xen::Color3f(1.0f, 1.0f, 1.0f);
	render_params.lights        = scene_lights;

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

	xen::Allocator*      alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear     arena  = xen::createArenaLinear(*alloc, xen::megabytes(32));
	xen::GraphicsDevice* device = xen::createRaytracerDebugDevice(arena);
	xen::Window*         app    = device->createWindow((Vec2u)window_size, "starfield");

	Vec3r mesh_verts[] = {
		Vec3r{ 0_r, 0_r, 0_r },
		Vec3r{ 1_r, 0_r, 0_r },
		Vec3r{ 0_r, 1_r, 0_r },

		Vec3r{ 0_r, 0_r, 0_r },
		Vec3r{ 1_r, 0_r, 0_r },
		Vec3r{ 0_r, 0_r, 1_r },
	};

	xen::FixedArray<xen::RenderCommand3d, 4> render_commands;
	xen::clearToZero(render_commands);
	render_commands[0].primative_type         = xen::PrimativeType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate              = xen::TestMeshGeometry_Axes;

	render_commands[1].primative_type         = xen::PrimativeType::LINES;
	render_commands[1].color                  = xen::Color::CYAN4f;
	render_commands[1].model_matrix           = (xen::Scale3d(200_r) *
	                                             xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	                                            );
	render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[1].immediate              = xen::TestMeshGeometry_UnitCubeLines;

	render_commands[2].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[2].color                  = xen::Color::MAGENTA4f;
	render_commands[2].model_matrix           = (xen::Scale3d(50_r) *
	                                             xen::Rotation3dy(90_deg)
	                                             // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r)
	                                            );
	render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[2].immediate.position     = &mesh_verts[0];
	render_commands[2].immediate.vertex_count = 3;

	render_commands[3].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[3].color                  = xen::Color::YELLOW4f;
	render_commands[3].model_matrix           = (xen::Scale3d(50_r)*
	                                             xen::Rotation3dy(90_deg)
	                                             // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r)
	                                            );
	render_commands[3].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[3].immediate.position     = &mesh_verts[3];
	render_commands[3].immediate.vertex_count = 3;

	xen::Aabb2u viewport = { 0, 0, (u32)window_size.x, (u32)window_size.y };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");
	while(xen::isWindowOpen(app)) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		scene_lights[0].point.position.y = xen::mapToRange(-1_r, 1_r, 0.01_r, 20.0_r, xen::sin(time * 90_deg));

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

		device->clear      (app, xen::Color::BLACK);
		device->render     (app, viewport, render_params, render_commands);
		device->swapBuffers(app);
	}
	printf("Exiting main loop\n");

	xen::destroyArenaLinear(*alloc, arena);
	delete alloc;

	return 0;
}
