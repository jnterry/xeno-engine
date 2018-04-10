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
	scene_lights[0].point.position = {10.0_r, 0.2_r, 10.0_r};
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

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Normal3r;
	vertex_spec[2] = xen::VertexAttribute::Color4b;

	Vec3r mesh_verts[] = {
		Vec3r::Origin,  Vec3r::UnitX,   Vec3r::UnitY,
		Vec3r::UnitX,   Vec3r::Origin,  Vec3r::UnitZ,
		Vec3r::UnitX,   Vec3r::Origin, -Vec3r::UnitZ,
	};

	xen::Color mesh_colors[] = {
		xen::Color::MAGENTA4f,
		xen::Color::MAGENTA4f,
		xen::Color::MAGENTA4f,

		xen::Color::YELLOW4f,
		xen::Color::YELLOW4f,
		xen::Color::YELLOW4f,

		xen::Color::CYAN4f,
		xen::Color::CYAN4f,
		xen::Color::CYAN4f,
	};

	xen::Mesh mesh_triangles  = app.device->createMesh(vertex_spec,
	                                                   XenArrayLength(mesh_colors),
	                                                   mesh_verts, nullptr, mesh_colors);
	xen::Mesh mesh_cube_lines = app.device->createMesh(vertex_spec,
	                                                   xen::TestMeshGeometry_UnitCubeLines);
	xen::Mesh mesh_axes       = app.device->createMesh(vertex_spec,
	                                                   xen::TestMeshGeometry_Axes);


	static_assert(XenArrayLength(mesh_verts) == XenArrayLength(mesh_colors),
	              "Expected equal number of positions and colors"
	             );

	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
	xen::clearToZero(render_commands);
	render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[0].color                  = xen::Color::WHITE4f;
	render_commands[0].model_matrix           = xen::Scale3d(100_r);
	render_commands[0].mesh                   = mesh_axes;

	render_commands[1].primitive_type         = xen::PrimitiveType::LINES;
	render_commands[1].color                  = xen::Color::CYAN4f;
	render_commands[1].model_matrix           = (xen::Scale3d(200_r) *
	                                             xen::Translation3d(-100.0_r, -100.0_r, -100.0_r)
	                                            );
	render_commands[1].mesh                   = mesh_cube_lines;

	render_commands[2].primitive_type         = xen::PrimitiveType::TRIANGLES;
	render_commands[2].color                  = xen::Color::WHITE4f;
	render_commands[2].model_matrix           = xen::Scale3d(50_r);
	render_commands[2].mesh                   = mesh_triangles;

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

		app.device->clear      (app.window, xen::Color{20,20,20,255});
		app.device->render     (app.window, viewport, render_params, render_commands);
		app.device->swapBuffers(app.window);
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
