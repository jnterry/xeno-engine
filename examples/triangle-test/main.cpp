#include <stdio.h>

#include <xen/graphics/TestMeshes.hpp>

#include "../common.cpp"

xen::RenderParameters3d render_params;

int main(int argc, char** argv){
	render_params.camera.z_near   =  0.001;
	render_params.camera.z_far    =  1000;
	render_params.camera.fov_y    =  70_deg;
	render_params.camera.look_dir = -Vec3r::UnitZ;
	render_params.camera.position =  Vec3r{0, 0, 10};

	ExampleApplication app = createApplication("triangle-test",
	                                           ExampleApplication::Backend::RASTERIZER
	                                          );

	Vec3r test_triangles_pos[] = {
		{ 0.0, 0.0, 0.0},   { 1.0, 0.0, 0.0},   { 0.0, 1.0, 0.0 },
		{ 0.0, 0.0, 5.0},   { 0.3, 0.0, 5.0},   { 0.0, 0.3, 5.0 },
		{ 0.0, 0.0, 0.0},   { 0.0, 0.3, 5.0},   { 0.0, 1.0, 0.0 },
		{-1.0, 3.0, 5.0},   {-1.0, 0.0, 3.0},   {-1.0, 1.0, 0.0 },
	};

	xen::Color test_triangles_color[] = {
		xen::Color::WHITE, xen::Color::RED, xen::Color::GREEN,
		{100, 255, 255, 255}, {255, 100, 255, 255}, {255, 255, 100, 255},
		xen::Color::YELLOW, xen::Color::RED, xen::Color::YELLOW,
		xen::Color::MAGENTA, xen::Color::YELLOW, xen::Color::CYAN,
	};

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
	xen::clearToZero(render_commands);

	render_commands[0].primative_type         = xen::PrimativeType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate              = xen::TestMeshGeometry_Axes;

	render_commands[1].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[1].color                  = xen::Color::WHITE4f;
	render_commands[1].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[1].immediate.position     = test_triangles_pos;
	render_commands[1].immediate.color        = test_triangles_color;
	render_commands[1].immediate.vertex_count = XenArrayLength(test_triangles_pos);

	render_commands[2].primative_type         = xen::PrimativeType::TRIANGLES;
	render_commands[2].color                  = xen::Color::WHITE4f;
	render_commands[2].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[2].immediate              = xen::TestMeshGeometry_UnitCube;

	// :TODO: get window size
	xen::Aabb2u viewport = { 0, 0, 100000, 100000 };

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
		handleCameraInputPlane(render_params.camera, dt);

		render_commands[2].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
		                                   xen::Rotation3dy(90_deg * time) *
		                                   xen::Translation3d(0_r, 3_r, 0_r)
		                                  );

		// Rendering
		app.device->clear      (app.window, xen::Color::BLACK);
		app.device->render     (app.window, viewport, render_params, render_commands);
		app.device->swapBuffers(app.window);
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
