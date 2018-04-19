#include <stdio.h>

#include "../common.cpp"

xen::Camera3dCylinder                  camera;
xen::RenderParameters3d                render_params;
xen::FixedArray<xen::LightSource3d, 3> scene_lights;

xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
xen::Mesh                                      mesh_torus_smooth;
xen::Mesh                                      mesh_torus_flat;
xen::Mesh                                      mesh_cube;
xen::Mesh                                      mesh_axes;
xen::Mesh                                      mesh_xzplane;

xen::FixedArray<xen::RenderCommand3d, 10> render_commands;

xen::sren::PostProcessorDisplayDepthBuffer pp_displayDepthBuffer;

xen::sren::PostProcessor* post_processors[] = {
	&pp_displayDepthBuffer,
};


#define CMD_IDX_TOR_A  0
#define CMD_IDX_TOR_B  1
#define CMD_IDX_FLOOR  2
#define CMD_IDX_STUDS  3
#define CMD_IDX_LIGHT  7

void initRenderCommands(){
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color           = xen::Color::WHITE4f;
	render_commands[0].model_matrix    = (xen::Translation3dx( 0.2_r) *
	                                      Mat4r::Identity
	                                     );
	render_commands[0].mesh            = mesh_torus_smooth;

	render_commands[1].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[1].color           = xen::Color::WHITE4f;
	render_commands[1].model_matrix    = (xen::Rotation3dx(90_deg) *
	                                      xen::Translation3dx(-0.2_r) *
	                                      Mat4r::Identity
	                                     );
	render_commands[1].mesh            = mesh_torus_flat;

	render_commands[2].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[2].color           = xen::Color::WHITE4f;
	render_commands[2].model_matrix    = (xen::Scale3d      (5, 5, 5) *
	                                      xen::Translation3d(0, -0.5_r, 0));
	render_commands[2].mesh            = mesh_xzplane;

	for(u32 i = 0; i < 4; ++i){
		xen::Color4f color = xen::Color::WHITE4f;
		switch(i){
		case 0: color = xen::Color::YELLOW4f;  break;
		case 1: color = xen::Color::MAGENTA4f; break;
		case 2: color = xen::Color::CYAN4f;    break;
		}

		Vec3r pos = Vec3r::Origin;
		switch(i){
		case 0: pos.x = -1_r; pos.z = -1_r; break;
		case 1: pos.x = -1_r; pos.z =  1_r; break;
		case 2: pos.x =  1_r; pos.z = -1_r; break;
		case 3: pos.x =  1_r; pos.z =  1_r; break;
		}
		pos   *= 2.0_r;
		pos.y -= 0.5_r;

		render_commands[CMD_IDX_STUDS+i].primitive_type  = xen::PrimitiveType::TRIANGLES;
		render_commands[CMD_IDX_STUDS+i].color           = color;
		render_commands[CMD_IDX_STUDS+i].emissive_color  = color;
		render_commands[CMD_IDX_STUDS+i].model_matrix    = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
		                                                    xen::Scale3d      (0.1_r) *
		                                                    xen::Translation3d(pos)
		                                                    );
		render_commands[CMD_IDX_STUDS+i].mesh            = mesh_cube;


	}

	for(u32 i = 0; i < 3; ++i){
		render_commands[CMD_IDX_LIGHT+i].primitive_type = xen::PrimitiveType::TRIANGLES;
		render_commands[CMD_IDX_LIGHT+i].color          = xen::Color::BLACK4f;
		render_commands[CMD_IDX_LIGHT+i].color[i]       = 1.0f;
		render_commands[CMD_IDX_LIGHT+i].model_matrix   = Mat4r::Identity;
		render_commands[CMD_IDX_LIGHT+i].mesh           = mesh_cube;
		render_commands[CMD_IDX_LIGHT+i].flags          = xen::Material::Flags::DisableShadowCast;
	}
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
	render_params.ambient_light = xen::Color3f(0.0f, 0.0f, 0.0f);

	for(u32 i = 0; i < 3; ++i){
		scene_lights[i].type           = xen::LightSource3d::POINT;
		scene_lights[i].point.position = Vec3r{0.0_r, 0.0_r, 0.0_r};
		scene_lights[i].point.position[i] = 1.0f;
		scene_lights[i].color          = xen::Color::BLACK4f;
		scene_lights[i].color[i]       = 1.0f;
		scene_lights[i].color.a        = 0.5f;
		scene_lights[i].attenuation    = {0.0f, 0.0f, 2.0f};
	}
	render_params.lights = scene_lights;
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
	mesh_torus_smooth = device->createMesh(mesh_data_torus);
	computeFlatNormals(mesh_data_torus);
	mesh_torus_flat = device->createMesh(mesh_data_torus);
	transaction.rollback();

	mesh_cube  = device->createMesh(vertex_spec,
	                                xen::TestMeshGeometry_UnitCube
	                               );
	mesh_axes = device->createMesh(vertex_spec,
	                               xen::TestMeshGeometry_Axes
	                              );

	mesh_xzplane = device->createMesh(vertex_spec,
	                                  xen::TestMeshGeometry_UnitXzPlaneCentered
	                                 );
}

int main(int argc, char** argv){
	initCamera();
	initSceneLights();

	ExampleApplication app = createApplication("torus",
	                                           ExampleApplication::Backend::RASTERIZER
	                                           //xen::makeArray(post_processors, XenArrayLength(post_processors))
	                                          );

	initMeshes(app.device, app.arena);
	initRenderCommands();

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

	pp_displayDepthBuffer.z_near            = camera.z_near;
	pp_displayDepthBuffer.z_far             = camera.z_far;
	pp_displayDepthBuffer.alpha             = 0.8f;
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
			case xen::WindowEvent::KeyPressed:
				switch(event->key.key){
				case xen::Key::W: // wireframe
					render_commands[0].primitive_type = xen::PrimitiveType::LINES;
					render_commands[1].primitive_type = xen::PrimitiveType::LINES;
					break;
				case xen::Key::P: // point cloud
					render_commands[0].primitive_type = xen::PrimitiveType::POINTS;
					render_commands[1].primitive_type = xen::PrimitiveType::POINTS;
					break;
				case xen::Key::F: // filled
					render_commands[0].primitive_type = xen::PrimitiveType::TRIANGLES;
					render_commands[1].primitive_type = xen::PrimitiveType::TRIANGLES;
					break;
				default: break;
				}
			default: break;
			}
		}
		handleCameraInputCylinder(camera, dt, 30);
		render_params.camera = xen::generateCamera3d(camera);

		for(u32 i = 0; i < 3; ++i){
			xen::Angle cycle = (100_deg * time * (1.0_r+i*0.1_r)) + 120_deg * i;
			Vec3r pos = xen::rotated(Vec3r{1,0,0},
			                         Vec3r::UnitY,
			                         cycle
			                        );

			pos.y += xen::sin(cycle + (120_deg*i)) * 0.5_r;
			scene_lights[i].point.position = pos;
			render_commands[CMD_IDX_LIGHT+i].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
			                                                 xen::Scale3d(0.05_r) *
			                                                 xen::Translation3d(pos)
			                                                 );
		}

		app.device->clear      (app.window, xen::Color{20, 20, 20, 255});
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);

	  fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
