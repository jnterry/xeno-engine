#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/time.hpp>
#include <xen/core/array.hpp>

#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/quaternion.hpp>

#include <xen/graphics/Image.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Light3d.hpp>
#include <xen/window/Window.hpp>

#include "../utilities.hpp"
//#include "../fragment_shaders.cpp"

const int CMD_BUNNY  = 0;
const int CMD_FLOOR  = 1;
const int CMD_LIGHT  = 2;
const int CMD_AXIS_X = 3;
const int CMD_AXIS_Y = 4;
const int CMD_AXIS_Z = 5;

struct State {
	xen::RenderParameters3d render_params;
	xen::FixedArray<xen::LightSource3d, 1> light_sources;
	xen::Camera3dCylinder camera;

	xen::Window*      window;
	xen::RenderTarget window_target;

	xen::Color4f point_light_color = xen::Color4f(1,0,0,1);
	xen::FixedArray<xen::VertexAttribute, 4> vertex_spec;

	const xen::Mesh* mesh_cube;
	const xen::Mesh* mesh_bunny;
	xen::Texture  texture_debug_img;

	const xen::Material* material_phong;
	const xen::Material* material_normal_lines;

	xen::FixedArray<xen::RenderCommand3d, 6> render_cmds;
};

State* state = nullptr;

void* init(const void* params){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();
	XenAssert(mod_ren != nullptr, "Graphics module must be loaded before cornell-box");
	XenAssert(mod_win != nullptr, "Window module must be loaded before cornell-box");

	xen::ArenaLinear& arena = xen::getThreadScratchSpace();

	state = (State*)xen::kernelAlloc(sizeof(State));
	xen::clearToZero(state);

	state->window        = mod_win->createWindow({800, 600}, "quicktest");
	state->window_target = mod_ren->createWindowRenderTarget(state->window);

	state->camera.z_near = 0.001;
	state->camera.z_far  = 10000;
	state->camera.fov_y  = 80_deg;
	state->camera.radius = 100;
	state->camera.height = 0;
	state->camera.up_dir = Vec3r::UnitY;
	state->camera.axis   = Vec3r::UnitY;
	state->camera.angle  = 0_deg;

	state->render_params.ambient_light = xen::Color3f(0.2, 0.2, 0.2);
	state->render_params.lights = state->light_sources;

	state->point_light_color = xen::Color4f(1,0,0,1);

	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;
	state->vertex_spec[3] = xen::VertexAttribute::TexCoord2f;

	state->material_phong        = mod_ren->createMaterial(material_creation_params_phong);
	state->material_normal_lines = mod_ren->createMaterial(material_creation_params_normal_lines);

	xen::RawImage test_image = xen::loadImage(arena, "resource/texture/test.bmp");
	state->texture_debug_img = mod_ren->createTexture(&test_image);

	state->mesh_cube         = mod_ren->createMesh(xen::TestMeshData_UnitCube);

	xen::MeshData* mesh_data_bunny = xen::createEmptyMeshData(arena, state->vertex_spec);
	xen::loadMeshFile(mesh_data_bunny, arena, "resource/mesh/bunny.obj", xen::MeshLoadFlags::CENTER_ORIGIN);
	state->mesh_bunny = mod_ren->createMesh(mesh_data_bunny);

	for(u64 i = 0; i < state->render_cmds.size; ++i){
		state->render_cmds[i].material = state->material_phong;
		state->render_cmds[i].material_params = xen::reserveBytes(
			arena, state->material_phong->parameters->size
		);
		xen::setMaterialParam(state->render_cmds[i], "emissive_color", xen::Color::BLACK4f);
		xen::setMaterialParam(state->render_cmds[i], "diffuse_color",  xen::Color::WHITE4f);
	}

	state->render_cmds[CMD_BUNNY ].model_matrix    = Mat4r::Identity;
	state->render_cmds[CMD_BUNNY ].mesh            = state->mesh_bunny;

	state->render_cmds[CMD_FLOOR ].model_matrix    = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                                  xen::Scale3d(60, 0.5, 60) *
	                                                  xen::Translation3d(0, -0.5_r, 0)
	                                                 );
	state->render_cmds[CMD_FLOOR ].mesh            = state->mesh_cube;
	state->render_cmds[CMD_FLOOR ].textures[0]     = state->texture_debug_img;

	state->render_cmds[CMD_LIGHT ].model_matrix    = Mat4r::Identity;
	state->render_cmds[CMD_LIGHT ].mesh            = state->mesh_cube;
	xen::setMaterialParam(state->render_cmds[CMD_LIGHT], "emissive_color", xen::Color::RED4f);
	xen::setMaterialParam(state->render_cmds[CMD_LIGHT], "diffuse_color",  xen::Color::RED4f);

	state->render_cmds[CMD_AXIS_X].model_matrix    = xen::Scale3d(15, 0.1, 0.1);
	state->render_cmds[CMD_AXIS_X].mesh            = state->mesh_cube;
	xen::setMaterialParam(state->render_cmds[CMD_AXIS_X], "emissive_color", xen::Color::RED4f);
	xen::setMaterialParam(state->render_cmds[CMD_AXIS_X], "diffuse_color",  xen::Color::RED4f);

	state->render_cmds[CMD_AXIS_Y].model_matrix    = xen::Scale3d(0.1, 15, 0.1);
	state->render_cmds[CMD_AXIS_Y].mesh            = state->mesh_cube;
  xen::setMaterialParam(state->render_cmds[CMD_AXIS_Y], "emissive_color", xen::Color::GREEN4f);
	xen::setMaterialParam(state->render_cmds[CMD_AXIS_Y], "diffuse_color",  xen::Color::GREEN4f);

	state->render_cmds[CMD_AXIS_Z].model_matrix    = xen::Scale3d(0.1, 0.1, 15);
	state->render_cmds[CMD_AXIS_Z].mesh            = state->mesh_cube;
	xen::setMaterialParam(state->render_cmds[CMD_AXIS_Z], "emissive_color", xen::Color::BLUE4f);
	xen::setMaterialParam(state->render_cmds[CMD_AXIS_Z], "diffuse_color",  xen::Color::BLUE4f);

	return state;
}

void* load( void* data, const void* params){
	state = (State*)data;
	return (void*)true;
}

void tick( const xen::TickContext& cntx){
	xen::ModuleApiWindow*   mod_win = (xen::ModuleApiWindow*  )xen::getModuleApi("window"  );
	xen::ModuleApiGraphics* mod_ren = (xen::ModuleApiGraphics*)xen::getModuleApi("graphics");
	XenAssert(mod_win != nullptr, "Expected window module to be loaded before quicktest");
	XenAssert(mod_ren != nullptr, "Expected graphics module to be loaded before quicktest");

	xen::Aabb2u viewport = { Vec2u::Origin, mod_win->getClientAreaSize(state->window) };

	Mat4r model_mat;

	real time = xen::asSeconds<real>(cntx.time);

	xen::WindowEvent* event;
	while((event = mod_win->pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
			mod_win->destroyWindow(state->window);
			xen::requestKernelShutdown();
			break;
		case xen::WindowEvent::Resized:
			viewport.max = event->resize.new_size;
			break;
		case xen::WindowEvent::KeyReleased:
			switch(event->key){
			case xen::Key::R:
				state->point_light_color.rgb = xen::Color3f(1,0,0);
				break;
			case xen::Key::G:
				state->point_light_color.rgb = xen::Color3f(0,1,0);
				break;
			case xen::Key::B:
				state->point_light_color.rgb = xen::Color3f(0,0,1);
				break;
			case xen::Key::W:
				state->point_light_color.rgb = xen::Color3f(1,1,1);
				break;
			case xen::Key::N:
				state->render_cmds[CMD_BUNNY].material = state->material_normal_lines;
				state->render_cmds[CMD_FLOOR].material = state->material_normal_lines;
				break;
			case xen::Key::M:
				state->render_cmds[CMD_BUNNY].material = state->material_phong;
				state->render_cmds[CMD_FLOOR].material = state->material_phong;
				break;
			default: break;
			}
			break;
		default: break;
		}
	}
	handleCameraInputCylinder(mod_win, state->window, state->camera, xen::asSeconds<real>(cntx.dt));

	Vec3r light_pos = xen::rotated(Vec3r{4, 3, 0}, Vec3r::UnitY, xen::Degrees(time*90_r));
	state->point_light_color.w = (1_r + sin(time*9)) / 2.0_r;

	////////////////////////////////////////////
	// Set render params
	state->render_params.camera = xen::generateCamera3d(state->camera);
	state->render_params.lights[0].point.position = light_pos;
	state->render_params.lights[0].color          = state->point_light_color;
	state->render_params.lights[0].attenuation    = {0.0f, 0.0f, 2.0f};

	////////////////////////////////////////////
	// Draw Cube Light
	model_mat  = Mat4r::Identity;
	model_mat *= xen::Rotation3dx(time * 41_deg);
	model_mat *= xen::Rotation3dy(time * 67_deg);
	model_mat *= xen::Rotation3dz(time * 83_deg);
	model_mat *= xen::Scale3d(0.3 + sin(time*15)*0.03,
	                          0.3 + sin(time*15 + 0.25*xen::PI)*0.03,
	                          0.3 + sin(time*15 + 0.5*xen::PI)*0.03);
	model_mat *= xen::Translation3d(light_pos);
	state->render_cmds[CMD_LIGHT].model_matrix   = model_mat;
	xen::setMaterialParam(state->render_cmds[CMD_LIGHT], "emissive_color", state->point_light_color);
	xen::setMaterialParam(state->render_cmds[CMD_LIGHT], "diffuse_color",  state->point_light_color);

	////////////////////////////////////////////
	// Draw Bunny
	model_mat = Mat4r::Identity;
	model_mat *= xen::Translation3d(0, 0.07, 0);
	model_mat *= xen::Scale3d(50);
	//model_mat *= xen::Rotation3dy(67_deg * time);
	state->render_cmds[CMD_BUNNY].model_matrix = model_mat;

	////////////////////////////////////////////
	// Do rendering
	mod_ren->clear      (state->window_target, xen::Color::BLACK);


	for(u64 i = 0; i < state->render_cmds.size; ++i){
		state->render_cmds[i].material = state->material_phong;
	}
	mod_ren->render     (state->window_target, viewport, state->render_params, state->render_cmds);
	for(u64 i = 0; i < state->render_cmds.size; ++i){
		state->render_cmds[i].material = state->material_normal_lines;
	}
	mod_ren->render     (state->window_target, viewport, state->render_params, state->render_cmds);

	mod_ren->swapBuffers(state->window_target);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
