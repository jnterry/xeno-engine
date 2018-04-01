#include <stdio.h>

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

	render_params.ambient_light = xen::Color3f(0.2f, 0.2f, 0.2f);
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

	ExampleApplication app = createApplication("raytracer-test",
	                                           ExampleApplication::Backend::RAYTRACER
	                                          );

	Vec3r mesh_verts[] = {
		Vec3r{ 0_r, 0_r, 0_r },
		Vec3r{ 1_r, 0_r, 0_r },
		Vec3r{ 0_r, 1_r, 0_r },

		Vec3r{ 1_r, 0_r, 0_r },
		Vec3r{ 0_r, 0_r, 0_r },
		Vec3r{ 0_r, 0_r, 1_r },
	};

	xen::Color mesh_colors[] = {
		xen::Color::MAGENTA4f,
		xen::Color::MAGENTA4f,
		xen::Color::MAGENTA4f,

		xen::Color::YELLOW4f,
		xen::Color::YELLOW4f,
		xen::Color::YELLOW4f,
	};

	static_assert(XenArrayLength(mesh_verts) == XenArrayLength(mesh_colors),
	              "Expected equal number of positions and colors"
	             );

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
	xen::clearToZero(render_commands);
	render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[0].immediate              = xen::TestMeshGeometry_Axes;

	render_commands[1].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[1].color                  = xen::Color::CYAN4f;
	render_commands[1].model_matrix           = (xen::Scale3d(200_r) *
	                                             xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	                                            );
	render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[1].immediate              = xen::TestMeshGeometry_UnitCubeLines;

	render_commands[2].primitive_type         = xen::PrimitiveType::TRIANGLES;
	render_commands[2].color                  = xen::Color::WHITE4f;
	render_commands[2].model_matrix           = (xen::Scale3d(50_r) *
	                                             xen::Rotation3dy(90_deg)
	                                             // * xen::Translation3d(-75.0_r, -75.0_r, -75.0_r)
	                                            );
	render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
	render_commands[2].immediate.position     = mesh_verts;
	render_commands[2].immediate.color        = mesh_colors;
	render_commands[2].immediate.vertex_count = XenArrayLength(mesh_verts);

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");
	while(xen::isWindowOpen(app.window)) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		scene_lights[0].point.position.y = xen::mapToRange(-1_r, 1_r, 0.01_r, 20.0_r, xen::sin(time * 90_deg));

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
