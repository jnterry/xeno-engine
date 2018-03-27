#include "../common.cpp"

#include <xen/graphics/TestMeshes.hpp>

xen::RenderParameters3d render_params;

#define LINE_COUNT_SQRT (50)
#define LINE_COUNT      (LINE_COUNT_SQRT * LINE_COUNT_SQRT)

int main(int argc, char** argv){
	render_params.camera.z_near   =  0.001;
	render_params.camera.z_far    =  1000;
	render_params.camera.fov_y    =  70_deg;
	render_params.camera.up_dir   =  Vec3r::UnitY;
	render_params.camera.look_dir = -Vec3r::UnitZ;
	render_params.camera.position =  Vec3r{0, 0, 50};

	ExampleApplication app = createApplication("line-test", ExampleApplication::Backend::RASTERIZER);

	Vec3r parallel_lines[LINE_COUNT * 2];
	for(int xi = 0; xi < LINE_COUNT_SQRT; ++xi){
		real x = (real)xi / (real)LINE_COUNT_SQRT;
		for(int yi = 0; yi < LINE_COUNT_SQRT; ++yi){
		  real y = (real)yi / (real)LINE_COUNT_SQRT;

			parallel_lines[(xi*LINE_COUNT_SQRT + yi) * 2 + 0] = {x, y, 0};
			parallel_lines[(xi*LINE_COUNT_SQRT + yi) * 2 + 1] = {x, y, 1};
		}
	}

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate              = xen::TestMeshGeometry_Axes;

	render_commands[1].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[1].color                  = xen::Color::YELLOW4f;
	render_commands[1].model_matrix           = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                             xen::Scale3d(50_r, 50_r, 10_r) *
	                                             xen::Rotation3dy( 30_deg)
	                                            );
	render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[1].immediate.position     = &parallel_lines[0];
	render_commands[1].immediate.vertex_count = LINE_COUNT * 2;

	render_commands[2].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[2].color                  = xen::Color::MAGENTA4f;
	render_commands[2].model_matrix           = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                             xen::Scale3d(50_r, 50_r, 10_r) *
	                                             xen::Rotation3dy(-30_deg)
	                                            );
	render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[2].immediate.position     = &parallel_lines[0];
	render_commands[2].immediate.vertex_count = LINE_COUNT * 2;

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
		handleCameraInputPlane(render_params.camera, dt);

		// Rendering
		app.device->clear      (app.window, xen::Color::BLACK);
		app.device->render     (app.window, viewport, render_params, render_commands);
		app.device->swapBuffers(app.window);
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
