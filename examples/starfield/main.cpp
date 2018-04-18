#include <stdio.h>

#include <xen/core/random.hpp>


#include "../common.cpp"

xen::RenderParameters3d render_params;
xen::Camera3dCylinder camera;
xen::FixedArray<xen::LightSource3d, 2> scene_lights;

const u32 STAR_COUNT = 1024;
Vec3r      star_positions[STAR_COUNT];
xen::Color star_colors   [STAR_COUNT];

int main(int argc, char** argv){
	camera.z_near = 0.001;
	camera.z_far  = 1000;
	camera.fov_y  = 70_deg;
	camera.radius = 450;
	camera.height = 0;
	camera.up_dir = Vec3r::UnitY;
	camera.axis   = Vec3r::UnitY;
	camera.target = Vec3r::Origin;
	camera.angle  = 0.0_deg;

	xen::clearToZero(&render_params);
	render_params.ambient_light = xen::Color3f(1.0f, 1.0f, 1.0f);

	ExampleApplication app = createApplication("starfield",
	                                           ExampleApplication::Backend::RASTERIZER
	                                           );

	for(u32 i = 0; i < STAR_COUNT; ++i){
		star_positions[i].x = xen::randf(-100, 100);
		star_positions[i].y = xen::randf(-100, 100);
		star_positions[i].z = xen::randf(-100, 100);

		star_colors[i].r = xen::mapToRange<real, u08>(-100, 100, 0, 255, star_positions[i].x);
		star_colors[i].g = xen::mapToRange<real, u08>(-100, 100, 0, 255, star_positions[i].y);
		star_colors[i].b = 255; //xen::mapToRange<real, u08>(-100, 100, 0, 255, star_positions[i].z);
		star_colors[i].a = 255;
	}

	xen::FixedArray<xen::VertexAttribute::Type, 2> vertex_spec;
	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Color4b;

	xen::Mesh mesh_stars = app.device->createMesh
		(vertex_spec, STAR_COUNT,
		 star_positions, star_colors);
	xen::Mesh mesh_axes = app.device->createMesh
		(vertex_spec, xen::TestMeshGeometry_Axes);
	xen::Mesh mesh_cube_lines = app.device->createMesh
		(vertex_spec, xen::TestMeshGeometry_UnitCubeLines);

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].mesh                   = mesh_axes;

	render_commands[1].primitive_type         = xen::PrimitiveType::POINTS;
	render_commands[1].color                  = xen::Color::WHITE4f;
	render_commands[1].model_matrix           = Mat4r::Identity;
	render_commands[1].mesh                   = mesh_stars;

	render_commands[2].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[2].color                  = xen::Color::CYAN4f;
	render_commands[2].model_matrix           = (xen::Scale3d(200_r) *
	                                             xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	                                            );
	render_commands[2].mesh                   = mesh_cube_lines;

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

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
		handleCameraInputCylinder(camera, dt);
		render_params.camera = xen::generateCamera3d(camera);

		for(u32 i = 0; i < STAR_COUNT; ++i){
			star_positions[i].z += dt * 75.0f;
			if(star_positions[i].z >= 100.0f){
				star_positions[i].z -= 200.0f;
			}
		}
		app.device->updateMeshAttribData(mesh_stars, 0, star_positions);

		app.device->clear      (app.window, xen::Color{20,20,20,255});
		app.device->render     (app.window, viewport, render_params, render_commands);
		app.device->swapBuffers(app.window);

		fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
