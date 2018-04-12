#include <stdio.h>

#include "../common.cpp"

xen::Camera3dCylinder                  camera;
xen::RenderParameters3d                render_params;
xen::FixedArray<xen::LightSource3d, 2> scene_lights;

xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
xen::Mesh                                      mesh_torus;
xen::Mesh                                      mesh_cube;
xen::Mesh                                      mesh_axes;

xen::FixedArray<xen::RenderCommand3d, 3> render_commands;

void initRenderCommands(){
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color           = xen::Color::WHITE4f;
	render_commands[0].model_matrix    = (xen::Translation3dx( 0.2_r) *
	                                      Mat4r::Identity
	                                     );
	render_commands[0].mesh            = mesh_torus;

	render_commands[1].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[1].color           = xen::Color::WHITE4f;
	render_commands[1].model_matrix    = (xen::Rotation3dx(90_deg) *
	                                      xen::Translation3dx(-0.2_r) *
	                                      Mat4r::Identity
	                                     );
	render_commands[1].mesh            = mesh_torus;
}

void initCamera(){
	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 10;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	camera.angle    = 0.0_deg;
}

void initSceneLights(){
	render_params.ambient_light = xen::Color3f(0.9f, 0.9f, 0.9f);
}

void initMeshes(xen::GraphicsDevice* device, xen::ArenaLinear& arena){
	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Normal3r;
	vertex_spec[2] = xen::VertexAttribute::Color4b;

	xen::MemoryTransaction transaction(arena);
	xen::MeshData* mesh_data_torus = xen::createEmptyMeshData(arena, vertex_spec);
	xen::loadMeshFile(mesh_data_torus, arena, "torus.obj",
	                  xen::MeshLoadFlags::CENTER_ORIGIN   |
	                  xen::MeshLoadFlags::SCALE_UNIT_SIZE
	                 );
	printf("Loaded torus mesh, %i faces\n", mesh_data_torus->vertex_count / 3);
	xen::Mesh mesh_torus = device->createMesh(mesh_data_torus);
	transaction.rollback();

	mesh_torus = device->createMesh(mesh_data_torus);

	mesh_cube  = device->createMesh(vertex_spec,
	                                xen::TestMeshGeometry_UnitCube
	                               );
	mesh_axes = device->createMesh(vertex_spec,
	                               xen::TestMeshGeometry_Axes
	                              );
}

int main(int argc, char** argv){
	ExampleApplication app = createApplication("torus",
	                                           ExampleApplication::Backend::RASTERIZER
	                                          );
	initCamera();
	initSceneLights();
	initMeshes(app.device, app.arena);
	initRenderCommands();

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");

	u32 frame_count = 0;
	xen::Stopwatch frame_timer;

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
		handleCameraInputCylinder(camera, dt, 30);
		render_params.camera = xen::generateCamera3d(camera);

		app.device->clear      (app.window, xen::Color{20, 20, 20, 255});
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);

	  ++frame_count;
	  if(xen::asSeconds<real>(frame_timer.getElapsedTime()) > 0.5_r){
		  real fps = frame_count / xen::asSeconds<real>(frame_timer.getElapsedTime());
		  printf("FPS: %f\n", fps);
		  frame_count = 0;
		  frame_timer.restart();
	  }
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
