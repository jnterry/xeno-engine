#include <stdio.h>

#include "../common.cpp"
#include <xen/graphics/Image.hpp>

xen::Camera3dCylinder                  camera;
xen::RenderParameters3d                render_params;
xen::FixedArray<xen::LightSource3d, 3> scene_lights;

xen::FixedArray<xen::VertexAttribute::Type, 4> vertex_spec;

xen::Mesh    mesh_xzplane;

xen::Texture texture_bricks_diffuse;

xen::FixedArray<xen::RenderCommand3d, 1> render_commands;

void initRenderCommands(){
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color           = xen::Color::WHITE4f;
	render_commands[0].model_matrix    = Mat4r::Identity;
	render_commands[0].mesh            = mesh_xzplane;
	render_commands[0].textures[0]     = texture_bricks_diffuse;
}

void initCamera(){
	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 3;
	camera.height   = 1;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	camera.angle    = 0.0_deg;
}

void initSceneLights(){
	xen::clearToZero(&render_params, sizeof(xen::RenderParameters3d));

	render_params.ambient_light = xen::Color3f(1.0f, 1.0f, 1.0f);
}

void initMeshes(xen::GraphicsDevice* device, xen::ArenaLinear& arena){
	xen::MemoryTransaction transaction(arena);

	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Normal3r;
	vertex_spec[2] = xen::VertexAttribute::Color4b;
	vertex_spec[3] = xen::VertexAttribute::TexCoord2f;

	mesh_xzplane = device->createMesh(vertex_spec,
	                                  xen::TestMeshGeometry_UnitXzPlaneCentered
	                                 );

	xen::RawImage bricks_image = xen::loadImage(arena, "bricks-diffuse.jpg");
	texture_bricks_diffuse = device->createTexture(&bricks_image);
}

int main(int argc, char** argv){
	initCamera();
	initSceneLights();

	ExampleApplication app = createApplication("torus",
	                                           ExampleApplication::Backend::RASTERIZER
	                                          );

	initMeshes(app.device, app.arena);
	initRenderCommands();

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

		handleCameraInputCylinder(camera, dt, 30);
		render_params.camera = xen::generateCamera3d(camera);

		app.device->clear      (app.window, xen::Color{20, 20, 20, 255});
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);

	  fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
