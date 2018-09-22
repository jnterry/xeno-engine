#include <stdio.h>

#include "../common.cpp"
#include "../fragment_shaders.cpp"
#include <xen/graphics/Image.hpp>
#include <xen/sren/FragmentShader.hpp>

xen::Camera3dCylinder                  camera;
xen::RenderParameters3d                render_params;
xen::FixedArray<xen::LightSource3d, 1> scene_lights;

xen::FixedArray<xen::VertexAttribute::Type, 4> vertex_spec;

xen::Mesh    mesh_xzplane;

xen::Texture texture_bricks_diffuse;
xen::Texture texture_bricks_normal;
xen::Texture texture_metal_diffuse;
xen::Texture texture_metal_normal;

xen::Shader  shader_normal_map;
xen::Shader  shader_phong;

xen::FixedArray<xen::RenderCommand3d, 1> render_commands;

void initRenderCommands(){
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type     = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color              = xen::Color::WHITE4f;
	render_commands[0].model_matrix       = Mat4r::Identity;
	render_commands[0].mesh               = mesh_xzplane;
	render_commands[0].textures[0]        = texture_bricks_diffuse;
	render_commands[0].textures[1]        = texture_bricks_normal;
	render_commands[0].specular_exponent  = 3_r;
	render_commands[0].specular_intensity = 0.1_r;
	render_commands[0].shader             = shader_normal_map;
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

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = Vec3r{0.5_r, 0.5_r, 0.0_r};
	scene_lights[0].color          = Vec4f{1.0f, 0.95f, 0.8f, 0.5f};
	scene_lights[0].attenuation    = {0.0f, 0.0f, 2.0f};

	render_params.ambient_light = xen::Color3f(0.1f, 0.1f, 0.1f);
	render_params.lights        = scene_lights;
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

	xen::RawImage image_bricks_diffuse = xen::loadImage(arena, "bricks-diffuse.jpg");
	xen::RawImage image_bricks_normal  = xen::loadImage(arena, "bricks-normal.jpg");
	xen::RawImage image_metal_diffuse  = xen::loadImage(arena, "metal-plate-diffuse.jpg");
	xen::RawImage image_metal_normal   = xen::loadImage(arena, "metal-plate-normal.jpg");

	texture_bricks_diffuse = device->createTexture(&image_bricks_diffuse);
	texture_bricks_normal  = device->createTexture(&image_bricks_normal);
	texture_metal_diffuse  = device->createTexture(&image_metal_diffuse);
	texture_metal_normal   = device->createTexture(&image_metal_normal);

	shader_normal_map = device->createShader((void*)&FragmentShader_NormalMap);
	shader_phong      = device->createShader((void*)&FragmentShader_Phong    );
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

		if(xen::isKeyPressed(xen::Key::Num1, app.window)){ // bricks
		  render_commands[0].textures[0]        = texture_bricks_diffuse;
		  render_commands[0].textures[1]        = texture_bricks_normal;
		  render_commands[0].specular_exponent  = 5_r;
		  render_commands[0].specular_intensity = 0.5_r;
		}
		if(xen::isKeyPressed(xen::Key::Num2, app.window)){ // metal
		  render_commands[0].textures[0]        = texture_metal_diffuse;
			render_commands[0].textures[1]        = texture_metal_normal;
		  render_commands[0].specular_exponent  = 100_r;
		  render_commands[0].specular_intensity = 5_r;
		}

		if(xen::isKeyPressed(xen::Key::Num9, app.window)){ // normal
			render_commands[0].shader = shader_normal_map;
		}
		if(xen::isKeyPressed(xen::Key::Num0, app.window)){ // phong
			render_commands[0].shader = shader_phong;
		}

		handleCameraInputCylinder(app.window, camera, dt, 30);
		render_params.camera = xen::generateCamera3d(camera);

		scene_lights[0].point.position = xen::rotated(Vec3r{0.5_r, 0.5_r, 0.0_r},
		                                              Vec3r::UnitY,
		                                              180_deg * time
		                                             );

		app.device->clear      (app.window, xen::Color{20, 20, 20, 255});
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);

	  fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
