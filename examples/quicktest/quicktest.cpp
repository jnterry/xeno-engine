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
#include <xen/graphics/Window.hpp>

#include "../utilities.hpp"
#include "../fragment_shaders.cpp"

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

	xen::Window* window;

	xen::Color4f point_light_color = xen::Color4f(1,0,0,1);
	xen::FixedArray<xen::VertexAttribute::Type, 4> vertex_spec;

	xen::Mesh     mesh_cube;
	xen::Mesh     mesh_bunny;
	xen::Texture  texture_debug_img;
	xen::Shader   shader_phong;

	xen::FixedArray<xen::RenderCommand3d , 6> render_cmds;
};

State* state = nullptr;

void* init(const void* params){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr, "Expected graphics module to be loaded before quicktest");

	xen::ArenaLinear& arena = xen::getThreadScratchSpace();

	state = (State*)xen::kernelAlloc(sizeof(State));
	xen::clearToZero(state);

	state->window = gmod->createWindow({800, 600}, "quicktest");

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

	state->shader_phong      = gmod->createShader({ (void*)&FragmentShader_Phong, nullptr, nullptr });

	xen::RawImage test_image = xen::loadImage(arena, "test.bmp");
	state->texture_debug_img = gmod->createTexture(&test_image);

	state->mesh_cube         = gmod->createMesh(state->vertex_spec, xen::TestMeshGeometry_UnitCube);

	xen::MeshData* mesh_data_bunny = xen::createEmptyMeshData(arena, state->vertex_spec);
	xen::loadMeshFile(mesh_data_bunny, arena, "bunny.obj", xen::MeshLoadFlags::CENTER_ORIGIN);
	state->mesh_bunny = gmod->createMesh(mesh_data_bunny);

	state->render_cmds[CMD_BUNNY ].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[CMD_BUNNY ].color           = xen::Color::RED4f;
	state->render_cmds[CMD_BUNNY ].model_matrix    = Mat4r::Identity;
	state->render_cmds[CMD_BUNNY ].mesh            = state->mesh_bunny;
	state->render_cmds[CMD_BUNNY ].shader          = state->shader_phong;

	state->render_cmds[CMD_FLOOR ].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[CMD_FLOOR ].color           = xen::Color::WHITE4f;
	state->render_cmds[CMD_FLOOR ].model_matrix    = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                                  xen::Scale3d(60, 0.5, 60) *
	                                                  xen::Translation3d(0, -0.5_r, 0)
	                                                 );
	state->render_cmds[CMD_FLOOR ].mesh            = state->mesh_cube;
	state->render_cmds[CMD_FLOOR ].textures[0]     = state->texture_debug_img;
	state->render_cmds[CMD_FLOOR ].shader          = state->shader_phong;

	state->render_cmds[CMD_LIGHT ].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[CMD_LIGHT ].color           = xen::Color::RED4f;
	state->render_cmds[CMD_LIGHT ].emissive_color  = xen::Color::RED4f;
	state->render_cmds[CMD_LIGHT ].model_matrix    = Mat4r::Identity;
	state->render_cmds[CMD_LIGHT ].mesh            = state->mesh_cube;

	state->render_cmds[CMD_AXIS_X].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[CMD_AXIS_X].color           = xen::Color::RED4f;
	state->render_cmds[CMD_AXIS_X].emissive_color  = xen::Color::RED4f;
	state->render_cmds[CMD_AXIS_X].model_matrix    = xen::Scale3d(15, 0.1, 0.1);
	state->render_cmds[CMD_AXIS_X].mesh            = state->mesh_cube;

	state->render_cmds[CMD_AXIS_Y].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[CMD_AXIS_Y].color           = xen::Color::GREEN4f;
	state->render_cmds[CMD_AXIS_Y].emissive_color  = xen::Color::GREEN4f;
	state->render_cmds[CMD_AXIS_Y].model_matrix    = xen::Scale3d(0.1, 15, 0.1);
	state->render_cmds[CMD_AXIS_Y].mesh            = state->mesh_cube;

	state->render_cmds[CMD_AXIS_Z].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[CMD_AXIS_Z].color           = xen::Color::BLUE4f;
	state->render_cmds[CMD_AXIS_Z].emissive_color  = xen::Color::BLUE4f;
	state->render_cmds[CMD_AXIS_Z].model_matrix    = xen::Scale3d(0.1, 0.1, 15);
	state->render_cmds[CMD_AXIS_Z].mesh            = state->mesh_cube;

	return state;
}

void* load( void* data, const void* params){
	state = (State*)data;
	return (void*)true;
}

void tick( const xen::TickContext& cntx){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr, "Expected graphics module to be loaded before quicktest");

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(state->window) };

	Mat4r model_mat;

	real time = xen::asSeconds<real>(cntx.time);

	xen::WindowEvent* event;
	while((event = xen::pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
		  gmod->destroyWindow(state->window);
			break;
		case xen::WindowEvent::Resized:
			viewport.max = event->resize.new_size;
			break;
		case xen::WindowEvent::KeyReleased:
			switch(event->key.key){
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
			default: break;
			}
			break;
		default: break;
		}
	}
	handleCameraInputCylinder(state->window, state->camera, xen::asSeconds<real>(cntx.dt));

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
	state->render_cmds[CMD_LIGHT].emissive_color = state->point_light_color;

	////////////////////////////////////////////
	// Draw Bunny
	model_mat = Mat4r::Identity;
	//model_mat *= xen::Rotation3dz(73_deg * time);
	model_mat *= xen::Scale3d(20);
	model_mat *= xen::Rotation3dy(67_deg * time);
	state->render_cmds[CMD_BUNNY].model_matrix = model_mat;

	////////////////////////////////////////////
	// Do rendering
	gmod->clear      (state->window, xen::Color::BLACK);
	gmod->render     (state->window, viewport, state->render_params, state->render_cmds);
	gmod->swapBuffers(state->window);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

xen::Module exported_xen_module = {
	xen::hash("game"),
	&init, &shutdown,
	&load, nullptr,
	&tick,
};
