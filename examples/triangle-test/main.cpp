#include <stdio.h>

#include <xen/graphics/TestMeshes.hpp>
#include <xen/sren/PostProcessor.hpp>

#include "../common.cpp"

xen::RenderParameters3d render_params;

xsr::PostProcessorAntialias          pp_antialias;
xsr::PostProcessorDisplayDepthBuffer pp_displayDepthBuffer;

xsr::PostProcessor* post_processors[] = {
	&pp_antialias,
	&pp_displayDepthBuffer,
};

int main(int argc, char** argv){
	render_params.camera.z_near   =  0.001;
	render_params.camera.z_far    =  1000;
	render_params.camera.fov_y    =  70_deg;
	render_params.camera.up_dir   =  Vec3r::UnitY;
	render_params.camera.look_dir = -Vec3r::UnitZ;
	render_params.camera.position =  Vec3r{0, 0, 10};

	pp_displayDepthBuffer.z_near   = render_params.camera.z_near;
	pp_displayDepthBuffer.z_far    = render_params.camera.z_far;
	pp_displayDepthBuffer.alpha    = 0.8f;
	pp_displayDepthBuffer.disabled = true;
	pp_antialias.disabled          = true;

	ExampleApplication app = createApplication("triangle-test",
	                                           ExampleApplication::Backend::RASTERIZER,
	                                           xen::makeArray(post_processors, XenArrayLength(post_processors))
	                                          );


	Vec3r test_triangles_pos[] = {
		{ 0.0_r, 0.0_r, 0.0_r},   { 1.0_r, 0.0_r, 0.0_r},   { 0.0_r, 1.0_r, 0.0_r },
		{ 0.0_r, 0.0_r, 5.0_r},   { 0.3_r, 0.0_r, 5.0_r},   { 0.0_r, 0.3_r, 5.0_r },
		{ 0.0_r, 0.0_r, 0.0_r},   { 0.0_r, 0.3_r, 5.0_r},   { 0.0_r, 1.0_r, 0.0_r },
		{-1.0_r, 3.0_r, 5.0_r},   {-1.0_r, 0.0_r, 3.0_r},   {-1.0_r, 1.0_r, 0.0_r },
	};

	xen::Color test_triangles_color[] = {
		xen::Color::WHITE, xen::Color::RED, xen::Color::GREEN,
		{100, 255, 255, 255}, {255, 100, 255, 255}, {255, 255, 100, 255},
		xen::Color::YELLOW, xen::Color::RED, xen::Color::YELLOW,
		xen::Color::MAGENTA, xen::Color::YELLOW, xen::Color::CYAN,
	};

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Normal3r;
	vertex_spec[2] = xen::VertexAttribute::Color4b;

	xen::Mesh mesh_triangles = app.device->createMesh
		(vertex_spec, XenArrayLength(test_triangles_pos),
		 test_triangles_pos, nullptr, test_triangles_color);
	xen::Mesh mesh_axes = app.device->createMesh
		(vertex_spec, xen::TestMeshGeometry_Axes);
	xen::Mesh mesh_cube = app.device->createMesh
		(vertex_spec, xen::TestMeshGeometry_UnitCube);

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].mesh                   = mesh_axes;

	render_commands[1].primitive_type         = xen::PrimitiveType::TRIANGLES;
	render_commands[1].color                  = xen::Color::WHITE4f;
	render_commands[1].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[1].mesh                   = mesh_triangles;

	render_commands[2].primitive_type         = xen::PrimitiveType::TRIANGLES;
	render_commands[2].color                  = xen::Color::WHITE4f;
	render_commands[2].model_matrix           = xen::Scale3d(1, 1, 1);
	render_commands[2].mesh                   = mesh_cube;

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

	pp_displayDepthBuffer.screen_region.min = Vec2u{viewport.max.x / 2, 10};
	pp_displayDepthBuffer.screen_region.max = Vec2u{viewport.max.x - 10, viewport.max.y / 2 - 10};

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");

	FpsCounter fps_counter;
	while(xen::isWindowOpen(app.window)) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		xen::WindowEvent* event;
		while((event = xen::pollEvent(app.window)) != nullptr){
			switch(event->type){
			case xen::WindowEvent::Closed:
				app.device->destroyWindow(app.window);
				break;
			default: break;
			}
		}
		handleCameraInputPlane(app.window, render_params.camera, dt);
		if(xen::isKeyPressed(xen::Key::Num1, app.window)){ pp_antialias.disabled          = false; }
		if(xen::isKeyPressed(xen::Key::Num2, app.window)){ pp_antialias.disabled          = true;  }
		if(xen::isKeyPressed(xen::Key::Num3, app.window)){ pp_displayDepthBuffer.disabled = false; }
		if(xen::isKeyPressed(xen::Key::Num4, app.window)){ pp_displayDepthBuffer.disabled = true;  }

		render_commands[2].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
		                                   xen::Rotation3dy(90_deg * time) *
		                                   xen::Translation3d(0_r, 3_r, 0_r)
		                                  );

		// Rendering
		app.device->clear      (app.window, xen::Color{20,20,20,255});
		app.device->render     (app.window, viewport, render_params, render_commands);
		app.device->swapBuffers(app.window);

		fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
