#include <stdio.h>

#include "../utilities.hpp"
#include "../fragment_shaders.cpp"

#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

struct State {
	xen::Camera3dCylinder                  camera;
	xen::RenderParameters3d                render_params;
	xen::FixedArray<xen::LightSource3d, 3> scene_lights;

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;

	xen::Window*      window;
	xen::RenderTarget window_target;

	xen::Mesh    mesh_torus_smooth;
	xen::Mesh    mesh_torus_flat;
	xen::Mesh    mesh_cube;
	xen::Mesh    mesh_axes;
	xen::Mesh    mesh_xzplane;

	xen::Shader  shader_phong;
	xen::Shader  shader_normals;
	xen::Shader  shader_positions;

	xen::FixedArray<xen::RenderCommand3d, 10> render_commands;
};

State* state = nullptr;

#define CMD_IDX_TOR_A  0
#define CMD_IDX_TOR_B  1
#define CMD_IDX_FLOOR  2
#define CMD_IDX_STUDS  3
#define CMD_IDX_LIGHT  7

void initRenderCommands(){
	xen::clearToZero(state->render_commands);

	state->render_commands[0].primitive_type     = xen::PrimitiveType::TRIANGLES;
	state->render_commands[0].color              = xen::Color::WHITE4f;
	state->render_commands[0].model_matrix       = (xen::Translation3dx( 0.2_r) *
	                                                Mat4r::Identity
	                                               );
	state->render_commands[0].mesh               = state->mesh_torus_smooth;
	state->render_commands[0].shader             = state->shader_phong;
	state->render_commands[0].specular_exponent  = 30_r;
	state->render_commands[0].specular_intensity = 2_r;

	state->render_commands[1].primitive_type     = xen::PrimitiveType::TRIANGLES;
	state->render_commands[1].color              = xen::Color::WHITE4f;
	state->render_commands[1].model_matrix       = (xen::Rotation3dx(90_deg) *
	                                                xen::Translation3dx(-0.2_r) *
	                                                Mat4r::Identity
	                                               );
	state->render_commands[1].mesh               = state->mesh_torus_flat;
	state->render_commands[1].shader             = state->shader_phong;
	state->render_commands[1].specular_exponent  = 30_r;
	state->render_commands[1].specular_intensity = 2_r;

	state->render_commands[2].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_commands[2].color           = xen::Color::WHITE4f;
	state->render_commands[2].model_matrix    = (xen::Scale3d      (5, 5, 5) *
	                                             xen::Translation3d(0, -0.5_r, 0));
	state->render_commands[2].mesh            = state->mesh_xzplane;

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
		pos.y += -0.499999_r;

		state->render_commands[CMD_IDX_STUDS+i].primitive_type  = xen::PrimitiveType::TRIANGLES;
		state->render_commands[CMD_IDX_STUDS+i].color           = color;
		state->render_commands[CMD_IDX_STUDS+i].emissive_color  = color;
		state->render_commands[CMD_IDX_STUDS+i].model_matrix    = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
		                                                           xen::Scale3d      (0.1_r) *
		                                                           xen::Translation3d(pos)
		                                                          );
		state->render_commands[CMD_IDX_STUDS+i].mesh            = state->mesh_cube;


	}

	for(u32 i = 0; i < 3; ++i){
		state->render_commands[CMD_IDX_LIGHT+i].primitive_type = xen::PrimitiveType::TRIANGLES;
		state->render_commands[CMD_IDX_LIGHT+i].color          = xen::Color::BLACK4f;
		state->render_commands[CMD_IDX_LIGHT+i].color[i]       = 1.0f;
		state->render_commands[CMD_IDX_LIGHT+i].emissive_color = state->render_commands[CMD_IDX_LIGHT+i].color;
		state->render_commands[CMD_IDX_LIGHT+i].model_matrix   = Mat4r::Identity;
		state->render_commands[CMD_IDX_LIGHT+i].mesh           = state->mesh_cube;
		state->render_commands[CMD_IDX_LIGHT+i].flags          = xen::Material::Flags::DisableShadowCast;
	}
}

void initCamera(){
	state->camera.z_near   = 0.001;
	state->camera.z_far    = 1000;
	state->camera.fov_y    = 70_deg;
	state->camera.radius   = 10;
	state->camera.height   = 0;
	state->camera.up_dir   = Vec3r::UnitY;
	state->camera.axis     = Vec3r::UnitY;
	state->camera.target   = Vec3r::Origin;
	state->camera.angle    = 0.0_deg;
}

void initSceneLights(){
	state->render_params.ambient_light = xen::Color3f(0.0f, 0.0f, 0.0f);

	for(u32 i = 0; i < 3; ++i){
		state->scene_lights[i].type           = xen::LightSource3d::POINT;
		state->scene_lights[i].point.position = Vec3r{0.0_r, 0.0_r, 0.0_r};
		state->scene_lights[i].point.position[i] = 1.0f;
		state->scene_lights[i].color          = xen::Color::BLACK4f;
		state->scene_lights[i].color[i]       = 1.0f;
		state->scene_lights[i].attenuation    = {0.0f, 0.0f, 2.0f};
	}
	state->render_params.lights = state->scene_lights;
}

void initMeshes(xen::ModuleApiGraphics* mod_ren){
	xen::ArenaLinear& arena = xen::getThreadScratchSpace();

	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;

	xen::MemoryTransaction transaction(arena);
	xen::MeshData* mesh_data_torus = xen::createEmptyMeshData(arena, state->vertex_spec);
	xen::loadMeshFile(mesh_data_torus, arena, "torus.obj",
	                  xen::MeshLoadFlags::CENTER_ORIGIN   |
	                  xen::MeshLoadFlags::SCALE_UNIT_SIZE
	                 );
	XenLogDone("Loaded torus mesh, %i faces", mesh_data_torus->vertex_count / 3);
	state->mesh_torus_smooth = mod_ren->createMesh(mesh_data_torus);
	computeFlatNormals(mesh_data_torus);
	state->mesh_torus_flat = mod_ren->createMesh(mesh_data_torus);

	state->mesh_cube  = mod_ren->createMesh(state->vertex_spec,
	                                     xen::TestMeshGeometry_UnitCube
	                                    );
	state->mesh_axes = mod_ren->createMesh(state->vertex_spec,
	                                    xen::TestMeshGeometry_Axes
	                                   );

	state->mesh_xzplane = mod_ren->createMesh(state->vertex_spec,
	                                       xen::TestMeshGeometry_UnitXzPlaneCentered
	                                      );

	state->shader_phong     = mod_ren->createShader({(void*)&FragmentShader_Phong    , nullptr, nullptr});
	state->shader_normals   = mod_ren->createShader({(void*)&FragmentShader_Normals  , nullptr, nullptr});
	state->shader_positions = mod_ren->createShader({(void*)&FragmentShader_Positions, nullptr, nullptr});
}

void* init( const void* params){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();
	XenAssert(mod_ren != nullptr, "Graphics module must be loaded before cornell-box");
	XenAssert(mod_win != nullptr, "Window module must be loaded before cornell-box");

	state = (State*)xen::kernelAlloc(sizeof(State));

	state->window        = mod_win->createWindow({600, 600}, "torus");
	state->window_target = mod_ren->createWindowRenderTarget(state->window);

	initCamera();
	initSceneLights();
	initMeshes(mod_ren);
	initRenderCommands();

	return state;
}

void* load( void* data, const void* params){
	state = (State*)data;
	return (void*)true;
}

void tick( const xen::TickContext& cntx){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();

	xen::Aabb2u viewport = { Vec2u::Origin, mod_win->getClientAreaSize(state->window) };

	xen::WindowEvent* event;
	while((event = mod_win->pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
			mod_win->destroyWindow(state->window);
			xen::requestKernelShutdown();
			break;
		default: break;
		}
	}

	// :TODO: these should use window events...
	if(mod_win->isKeyPressed(xen::Key::Num1)){ // points
		state->render_commands[0].primitive_type = xen::PrimitiveType::POINTS;
	}
	if(mod_win->isKeyPressed(xen::Key::Num2)){ // lines
		state->render_commands[0].primitive_type = xen::PrimitiveType::LINES;
	}
	if(mod_win->isKeyPressed(xen::Key::Num3)){ // triangles
			state->render_commands[0].primitive_type = xen::PrimitiveType::TRIANGLES;
	}
	if(mod_win->isKeyPressed(xen::Key::Num4)){ // normals
		state->render_commands[0].shader = state->shader_normals;
		state->render_commands[1].shader = state->shader_normals;
	}
	if(mod_win->isKeyPressed(xen::Key::Num5)){ // world positions
		state->render_commands[0].shader = state->shader_positions;
		state->render_commands[1].shader = state->shader_positions;
	}
	if(mod_win->isKeyPressed(xen::Key::Num6)){ // Shaded
		state->render_commands[0].shader = state->shader_phong;
		state->render_commands[1].shader = state->shader_phong;
	}
	if(mod_win->isKeyPressed(xen::Key::Num7)){ // Basic Shaded
		state->render_commands[0].shader = xen::makeNullGraphicsHandle<xen::Shader>();
		state->render_commands[1].shader = xen::makeNullGraphicsHandle<xen::Shader>();
	}

	handleCameraInputCylinder(mod_win, state->window, state->camera, xen::asSeconds<real>(cntx.dt), 30);
	state->render_params.camera = xen::generateCamera3d(state->camera);

	for(u32 i = 0; i < 3; ++i){
		xen::Angle cycle = (100_deg * xen::asSeconds<real>(cntx.time) * (1.0_r+i*0.1_r)) + 120_deg * i;
		Vec3r pos = xen::rotated(Vec3r{1,0,0},
		                         Vec3r::UnitY,
		                         cycle
		                         );

		pos.y += xen::sin(cycle + (120_deg*i)) * 0.5_r;
		state->scene_lights[i].point.position = pos;
		state->render_commands[CMD_IDX_LIGHT+i].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
		                                                        xen::Scale3d(0.05_r) *
		                                                        xen::Translation3d(pos)
		                                                       );
	}

  mod_ren->clear      (state->window_target, xen::Color{20, 20, 20, 255});
  mod_ren->render     (state->window_target, viewport, state->render_params, state->render_commands);
  mod_ren->swapBuffers(state->window_target);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
