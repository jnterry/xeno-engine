#include <stdio.h>

#include "../utilities.hpp"
//#include "../fragment_shaders.cpp"

#include <xen/math/quaternion.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

struct State {
	xen::ArenaLinear arena;

	xen::Camera3dCylinder                  camera;
	xen::RenderParameters3d                render_params;
	xen::FixedArray<xen::LightSource3d, 3> scene_lights;

	xen::FixedArray<xen::VertexAttribute, 3> vertex_spec;

	xen::Window*      window;
	xen::RenderTarget window_target;

	const xen::Mesh* mesh_torus_smooth;
	const xen::Mesh* mesh_torus_flat;
	const xen::Mesh* mesh_cube;
	const xen::Mesh* mesh_axes;
	const xen::Mesh* mesh_xzplane;

	const xen::Material* mat_phong;

	xen::FixedArray<xen::RenderCommand3d, 10> render_cmds;
};

State* state = nullptr;

#define CMD_IDX_TOR_A  0
#define CMD_IDX_TOR_B  1
#define CMD_IDX_FLOOR  2
#define CMD_IDX_STUDS  3
#define CMD_IDX_LIGHT  7

void initRenderCommands(xen::ModuleApiGraphics* mod_ren){
	state->mat_phong = mod_ren->createMaterial(material_creation_params_phong);

	xen::clearToZero(state->render_cmds);

	for(unsigned i = 0; i < state->render_cmds.size; ++i){
		state->render_cmds[i].material_params = xen::reserveBytes(
			state->arena, state->mat_phong->parameters->size
		);
		xen::clearToZero(
			state->render_cmds[i].material_params, state->mat_phong->parameters->size
		);

		state->render_cmds[i].material = state->mat_phong;
	  xen::setMaterialParam(state->render_cmds[i], "diffuse_color", xen::Color::WHITE4f);
	}

	state->render_cmds[0].model_matrix       = (xen::Translation3dx( 0.2_r) *
	                                            Mat4r::Identity);
	state->render_cmds[0].mesh               = state->mesh_torus_smooth;
	xen::setMaterialParam(state->render_cmds[0], "specular_exponent",  30_r);
	xen::setMaterialParam(state->render_cmds[0], "specular_intensity",  2_r);

	state->render_cmds[1].model_matrix       = (xen::Rotation3dx(90_deg) *
	                                                xen::Translation3dx(-0.2_r) *
	                                                Mat4r::Identity
	                                               );
	state->render_cmds[1].mesh               = state->mesh_torus_flat;
	xen::setMaterialParam(state->render_cmds[1], "specular_exponent",  30_r);
	xen::setMaterialParam(state->render_cmds[1], "specular_intensity",  2_r);

	state->render_cmds[2].model_matrix    = (xen::Scale3d      (5, 5, 5) *
	                                         xen::Translation3d(0, -0.5_r, 0));
	state->render_cmds[2].mesh            = state->mesh_xzplane;

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

		state->render_cmds[CMD_IDX_STUDS+i].model_matrix    = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
		                                                       xen::Scale3d      (0.1_r) *
		                                                       xen::Translation3d(pos)
		                                                          );
		state->render_cmds[CMD_IDX_STUDS+i].mesh            = state->mesh_cube;
		xen::setMaterialParam(state->render_cmds[CMD_IDX_STUDS+i], "diffuse_color",  color);
		xen::setMaterialParam(state->render_cmds[CMD_IDX_STUDS+i], "emissive_color", color);
	}

	for(u32 i = 0; i < 3; ++i){
		state->render_cmds[CMD_IDX_LIGHT+i].model_matrix   = Mat4r::Identity;
		state->render_cmds[CMD_IDX_LIGHT+i].mesh           = state->mesh_cube;
		state->render_cmds[CMD_IDX_LIGHT+i].disable_shadow_cast = true;

		xen::Color4f color = xen::Color::BLACK4f;
		color[i] = 1.0f;

		xen::setMaterialParam(state->render_cmds[CMD_IDX_STUDS+i], "diffuse_color",  color);
		xen::setMaterialParam(state->render_cmds[CMD_IDX_STUDS+i], "emissive_color", color);
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
	xen::loadMeshFile(mesh_data_torus, arena, "resource/mesh/torus.obj",
	                  xen::MeshLoadFlags::CENTER_ORIGIN   |
	                  xen::MeshLoadFlags::SCALE_UNIT_SIZE
	                 );
	XenLogDone("Loaded torus mesh, %i faces", mesh_data_torus->vertex_count / 3);
	state->mesh_torus_smooth = mod_ren->createMesh(mesh_data_torus);
	computeFlatNormals(mesh_data_torus);
	state->mesh_torus_flat = mod_ren->createMesh(mesh_data_torus);

	state->mesh_cube    = mod_ren->createMesh(xen::TestMeshData_UnitCube);
	state->mesh_axes    = mod_ren->createMesh(xen::TestMeshData_Axes);
	state->mesh_xzplane = mod_ren->createMesh(xen::TestMeshData_UnitXzPlaneCentered);
}

void* init( const void* params){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();
	XenAssert(mod_ren != nullptr, "Graphics module must be loaded before cornell-box");
	XenAssert(mod_win != nullptr, "Window module must be loaded before cornell-box");

	state = (State*)xen::kernelAlloc(sizeof(State) + xen::kilobytes(1));
	state->arena.start = xen::ptrGetAdvanced(state, sizeof(State));
	state->arena.end   = xen::ptrGetAdvanced(state->arena.start, xen::kilobytes(1));
	state->arena.next_byte = state->arena.start;

	state->window        = mod_win->createWindow({600, 600}, "torus");
	state->window_target = mod_ren->createWindowRenderTarget(state->window);

	initCamera();
	initSceneLights();
	initMeshes(mod_ren);
	initRenderCommands(mod_ren);

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
		case xen::WindowEvent::KeyPressed:
			switch(event->key){
			case xen::Key::Num1: // points
				state->render_cmds[0].draw_mode = xen::RenderCommand3d::PointCloud;
				break;
			case xen::Key::Num2: // lines
				state->render_cmds[0].draw_mode = xen::RenderCommand3d::Wireframe;
				break;
			case xen::Key::Num3: // triangles
				state->render_cmds[0].draw_mode = xen::RenderCommand3d::Filled;
				break;
			default: break;
			}
		default: break;
		}
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
		state->render_cmds[CMD_IDX_LIGHT+i].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
		                                                        xen::Scale3d(0.05_r) *
		                                                        xen::Translation3d(pos)
		                                                       );
	}

	mod_ren->clear      (state->window_target, xen::Color{20, 20, 20, 255});
	mod_ren->render     (state->window_target, viewport, state->render_params, state->render_cmds);
	mod_ren->swapBuffers(state->window_target);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
