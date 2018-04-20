#include <stdio.h>

#include "../common.cpp"
#include "cornell-box.hpp"
#include <xen/graphics/Image.hpp>

// Locations for the boxes in the cornell scene
const Vec3r tall_box_center  = {-0.15_r,  0.0_r, -0.10_r};
const Vec3r short_box_center = { 0.18_r,  0.0_r,  0.18_r};

xen::Camera3dCylinder                  camera;
xen::RenderParameters3d                render_params;
xen::FixedArray<xen::LightSource3d, 2> scene_lights;
xen::sren::PostProcessorDepthFog       pp_fog;

xen::sren::PostProcessor* post_processors[] = {
	&pp_fog,
};

xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
xen::Mesh                                      mesh_cornell_walls;
xen::Mesh                                      mesh_cube;
xen::Mesh                                      mesh_axes;

xen::FixedArray<xen::RenderCommand3d, 5> render_commands;

void initRenderCommands(){
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color           = xen::Color::WHITE4f;
	render_commands[0].model_matrix    = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
	                                      xen::Rotation3dy(180_deg)
	                                      );
	render_commands[0].mesh            = mesh_cornell_walls;
	// There is nothing outside of the cornell box, don't cast shadows for speed
	render_commands[0].flags           = xen::RenderCommand3d::Flags::DisableShadowCast;

	render_commands[1].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[1].color           = Vec4f{ 0.15f, 0.15f, 0.75f, 1.0f };
	render_commands[1].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r) *
	                                      xen::Scale3d      (0.3_r, 0.6_r, 0.3_r  ) *
	                                      xen::Rotation3dy  (15_deg               ) *
	                                      xen::Translation3d(tall_box_center      )
	                                     );
	render_commands[1].mesh            = mesh_cube;

	render_commands[2].primitive_type  = xen::PrimitiveType::TRIANGLES;
  render_commands[2].color           = Vec4f{ 0.75f, 0.15f, 0.15f, 1.0_r };
	render_commands[2].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r) *
	                                      xen::Scale3d      (0.3_r, 0.3_r, 0.3_r  ) *
	                                      xen::Rotation3dy  (-18_deg              ) *
	                                      xen::Translation3d(short_box_center     )
	                                     );
	render_commands[2].mesh            = mesh_cube;

	render_commands[3].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[3].color           = xen::Color::YELLOW4f;
	render_commands[3].emissive_color  = xen::Color::YELLOW4f;
	render_commands[3].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r  ) *
	                                      xen::Scale3d      (0.05_r, 0.05_r, 0.05_r ) *
	                                      xen::Rotation3dy  (18_deg                 ) *
	                                      xen::Translation3d(-0.1_r, 0.0_r, 0.2_r    )
	                                     );
	render_commands[3].mesh            = mesh_cube;

	// Light source
	render_commands[4].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[4].color           = xen::Color::RED4f;
	render_commands[4].model_matrix    = Mat4r::Identity;
	render_commands[4].mesh            = mesh_cube;
	// This is geometry around a light source - don't block the emitted light!
	render_commands[4].flags           = xen::RenderCommand3d::Flags::DisableShadowCast;
}

void initCamera(){
	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 10;
	camera.height   = 0.001;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = {0.0_r, 0.5_r, 0.0_r};
	camera.angle    = 0.0_deg;
}

void initSceneLights(){
	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = Vec3r{0.0_r, 1.0_r, 0.0_r};
	scene_lights[0].color          = xen::Color::WHITE4f;
	scene_lights[0].color.a        = 0.2f;
	scene_lights[0].attenuation    = {0.0f, 0.0f, 2.0f};

	scene_lights[1].type           = xen::LightSource3d::POINT;
	scene_lights[1].point.position = Vec3r{0.0_r, 0.5_r, 0.0_r};
	scene_lights[1].color          = xen::Color::RED4f;
	scene_lights[1].color.a        = 0.05f;
	scene_lights[1].attenuation    = {0.0f, 0.0f, 2.0f};

	render_params.ambient_light = xen::Color3f(0.1f, 0.1f, 0.1f);
	render_params.lights        = scene_lights;
}

void initMeshes(ExampleApplication& app){
	xen::MemoryTransaction transaction(app.arena);

	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Normal3r;
	vertex_spec[2] = xen::VertexAttribute::Color4b;

	mesh_cornell_walls = app.device->createMesh(vertex_spec,
	                                            MeshGeometry_CornellBoxWalls
	                                           );
	mesh_cube = app.device->createMesh(vertex_spec,
	                                   xen::TestMeshGeometry_UnitCube
	                                  );
	mesh_axes = app.device->createMesh(vertex_spec,
	                                   xen::TestMeshGeometry_Axes
	                                  );
}

int main(int argc, char** argv){
	ExampleApplication app = createApplication("cornell-box",
	                                           ExampleApplication::Backend::RASTERIZER,
																						 xen::makeArray(post_processors, XenArrayLength(post_processors))
	                                          );
	initCamera();
	initSceneLights();
	initMeshes(app);
	initRenderCommands();

	pp_fog.fog_color = xen::Color4f{1.0,1.0,1.0,0.8};
	pp_fog.z_near = render_params.camera.z_near+0.1;
	pp_fog.z_far  = 4.0;

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
		handleCameraInputCylinder(camera, dt, 20);
		render_params.camera = xen::generateCamera3d(camera);

		if(xen::isKeyPressed(xen::Key::F)){
			pp_fog.fog_color = xen::Color4f{1.0,1.0,1.0,0.8};
		}
		if(xen::isKeyPressed(xen::Key::G)){
		  pp_fog.fog_color = xen::Color4f{1.0,1.0,1.0,0.0};
		}

		Vec3r light_1_pos = (tall_box_center +
		                     xen::rotated(Vec3r{0.3_r, 0.5_r, 0.0_r}, Vec3r::UnitY, 90_deg * time)
		                    );
		render_commands[4].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
		                                   xen::Scale3d(0.03_r) *
		                                   xen::Translation3d(light_1_pos)
		                                  );
		scene_lights[1].point.position = light_1_pos;

		app.device->clear      (app.window, xen::Color{20, 20, 20, 255});
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);

	  fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
