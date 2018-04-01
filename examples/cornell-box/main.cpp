#include <stdio.h>

#include "../common.cpp"
#include "cornell-box.hpp"

xen::Camera3dCylinder camera;
xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 10;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = {0.0_r, 0.5_r, 0.0_r};
	camera.angle    = 0.0_deg;

	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = {0.0_r, 1.0_r, 0.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f;
	scene_lights[0].color.a        = 0.4f;
	scene_lights[0].attenuation    = {0.0f, 0.0f, 4.0f};

	render_params.ambient_light = xen::Color3f(0.1f, 0.1f, 0.1f);
	render_params.lights        = scene_lights;

	ExampleApplication app = createApplication("cornell-box",
	                                           ExampleApplication::Backend::RASTERIZER
	                                          );

	xen::FixedArray<xen::RenderCommand3d, 4> render_commands;
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate              = xen::TestMeshGeometry_Axes;

	render_commands[1].primitive_type         = xen::PrimitiveType::TRIANGLES;
	render_commands[1].color                  = xen::Color::WHITE4f;
	render_commands[1].model_matrix           = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
	                                             xen::Rotation3dy(180_deg)
	                                             );
	render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[1].immediate              = MeshGeometry_CornellBoxWalls;

	render_commands[2].primitive_type = xen::PrimitiveType::TRIANGLES;
	render_commands[2].color                  = xen::Color::BLUE4f;
	render_commands[2].model_matrix           = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
	                                             xen::Scale3d      (0.3_r, 0.6_r, 0.3_r  ) *
	                                             xen::Rotation3dy  (15_deg               ) *
	                                             xen::Translation3d(-0.15_r, 0.0_r, -0.10_r)
	                                            );
	render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[2].immediate              = xen::TestMeshGeometry_UnitCube;

	render_commands[3].primitive_type = xen::PrimitiveType::TRIANGLES;
	render_commands[3].color                  = xen::Color::RED4f;
	render_commands[3].model_matrix           = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
	                                             xen::Scale3d      (0.3_r, 0.3_r, 0.3_r  ) *
	                                             xen::Rotation3dy  (-18_deg               ) *
	                                             xen::Translation3d(0.18_r, 0.0_r, 0.18_r)
	                                            );
	render_commands[3].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[3].immediate              = xen::TestMeshGeometry_UnitCube;





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
		handleCameraInputCylinder(camera, dt, 20);
		render_params.camera = xen::generateCamera3d(camera);

		app.device->clear      (app.window, xen::Color::BLACK);
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
