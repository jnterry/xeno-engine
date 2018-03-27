#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/core/time.hpp>
#include <xen/core/array.hpp>
#include <xen/core/File.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Window.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include "../common.cpp"
#include "cornell-box.hpp"

xen::Camera3dCylinder camera;
xen::RenderParameters3d render_params;

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

	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = {1.0_r, 1.0_r, 1.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f;
	scene_lights[0].attenuation    = {0.0f, 0.0f, 1.0f};

	render_params.ambient_light = xen::Color3f(0.3f, 0.3f, 0.3f);
	render_params.lights        = scene_lights;

	ExampleApplication app = createApplication("cornell-box",
	                                           ExampleApplication::Backend::RASTERIZER
	                                           );

	xen::FixedArray<xen::RenderCommand3d, 1> render_commands;
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type         = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Translation3d(-277_r, -277_r, -277_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate              = MeshGeometry_CornellBox;

	Vec2u window_size = xen::getClientAreaSize(app.window);
	printf("window size is %u, %u\n", window_size.x, window_size.y);

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");
	while(xen::isWindowOpen(app.window)) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		printf("dt: %f\n", dt);

		xen::WindowEvent* event;
		while((event = xen::pollEvent(app.window)) != nullptr){
			switch(event->type){
			case xen::WindowEvent::Closed:
				app.device->destroyWindow(app.window);
				break;
			default: break;
			}
		}
		handleCameraInputCylinder(camera, dt);
		render_params.camera = xen::generateCamera3d(camera);

		app.device->clear      (app.window, xen::Color::BLACK);
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
