#include <stdio.h>

#include "../utilities.hpp"
#include "../fragment_shaders.cpp"
#include <xen/graphics/Image.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/sren/FragmentShader.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

struct State{
	xen::Camera3dCylinder                  camera;
	xen::RenderParameters3d                render_params;
	xen::FixedArray<xen::LightSource3d, 1> scene_lights;

	xen::FixedArray<xen::VertexAttribute::Type, 4> vertex_spec;

	xen::Mesh    mesh_xzplane;

	xen::Window* window;

	xen::Texture texture_bricks_diffuse;
	xen::Texture texture_bricks_normal;
	xen::Texture texture_metal_diffuse;
	xen::Texture texture_metal_normal;

	xen::Shader  shader_normal_map;
	xen::Shader  shader_phong;

	xen::FixedArray<xen::RenderCommand3d, 1> render_commands;
};

State* state = nullptr;

void initRenderCommands(){
	xen::clearToZero(state->render_commands);

	state->render_commands[0].primitive_type     = xen::PrimitiveType::TRIANGLES;
	state->render_commands[0].color              = xen::Color::WHITE4f;
	state->render_commands[0].model_matrix       = Mat4r::Identity;
	state->render_commands[0].mesh               = state->mesh_xzplane;
	state->render_commands[0].textures[0]        = state->texture_bricks_diffuse;
	state->render_commands[0].textures[1]        = state->texture_bricks_normal;
	state->render_commands[0].specular_exponent  = 3_r;
	state->render_commands[0].specular_intensity = 0.1_r;
	state->render_commands[0].shader             = state->shader_normal_map;
}

void initCamera(){
	state->camera.z_near   = 0.001;
	state->camera.z_far    = 1000;
	state->camera.fov_y    = 70_deg;
	state->camera.radius   = 3;
	state->camera.height   = 1;
	state->camera.up_dir   = Vec3r::UnitY;
	state->camera.axis     = Vec3r::UnitY;
	state->camera.target   = Vec3r::Origin;
	state->camera.angle    = 0.0_deg;
}

void initSceneLights(){
	xen::clearToZero(&state->render_params);

	state->scene_lights[0].type           = xen::LightSource3d::POINT;
	state->scene_lights[0].point.position = Vec3r{0.5_r, 0.5_r, 0.0_r};
	state->scene_lights[0].color          = Vec4f{1.0f, 0.95f, 0.8f, 0.5f};
	state->scene_lights[0].attenuation    = {0.0f, 0.0f, 2.0f};

	state->render_params.ambient_light = xen::Color3f(0.1f, 0.1f, 0.1f);
	state->render_params.lights        = state->scene_lights;
}

void initMeshes(xen::GraphicsModuleApi* gmod){
	xen::ArenaLinear& arena = xen::getThreadScratchSpace();

	xen::MemoryTransaction transaction(arena);

	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;
	state->vertex_spec[3] = xen::VertexAttribute::TexCoord2f;

	state->mesh_xzplane = gmod->createMesh(state->vertex_spec,
	                                       xen::TestMeshGeometry_UnitXzPlaneCentered
	                                      );

	xen::RawImage image_bricks_diffuse = xen::loadImage(arena, "bricks-diffuse.jpg");
	xen::RawImage image_bricks_normal  = xen::loadImage(arena, "bricks-normal.jpg");
	xen::RawImage image_metal_diffuse  = xen::loadImage(arena, "metal-plate-diffuse.jpg");
	xen::RawImage image_metal_normal   = xen::loadImage(arena, "metal-plate-normal.jpg");

	state->texture_bricks_diffuse = gmod->createTexture(&image_bricks_diffuse);
	state->texture_bricks_normal  = gmod->createTexture(&image_bricks_normal);
	state->texture_metal_diffuse  = gmod->createTexture(&image_metal_diffuse);
	state->texture_metal_normal   = gmod->createTexture(&image_metal_normal);

	state->shader_normal_map = gmod->createShader({ (void*)&FragmentShader_NormalMap });
	state->shader_phong      = gmod->createShader({ (void*)&FragmentShader_Phong     });
}

void* init( const void* params){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr, "Expected graphics module to be loaded before texture-test");

	state = (State*)xen::kernelAlloc(sizeof(State));

	state->window = gmod->createWindow({800, 600}, "texture-test");

	initCamera();
	initSceneLights();
	initMeshes(gmod);
	initRenderCommands();

	return state;
}

void* load( void* data, const void* params){
	state = (State*)data;
	return (void*)true;
}

void tick( const xen::TickContext& cntx){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr, "Expected graphics module to be loaded before texture-test");

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(state->window) };

	xen::WindowEvent* event;
	while((event = xen::pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
		  gmod->destroyWindow(state->window);
			xen::requestKernelShutdown();
			break;
		default: break;
		}
	}

	if(xen::isKeyPressed(xen::Key::Num1, state->window)){ // bricks
		state->render_commands[0].textures[0]        = state->texture_bricks_diffuse;
		state->render_commands[0].textures[1]        = state->texture_bricks_normal;
		state->render_commands[0].specular_exponent  = 5_r;
		state->render_commands[0].specular_intensity = 0.5_r;
	}
	if(xen::isKeyPressed(xen::Key::Num2, state->window)){ // metal
		state->render_commands[0].textures[0]        = state->texture_metal_diffuse;
		state->render_commands[0].textures[1]        = state->texture_metal_normal;
		state->render_commands[0].specular_exponent  = 100_r;
		state->render_commands[0].specular_intensity = 5_r;
	}

	if(xen::isKeyPressed(xen::Key::Num9, state->window)){ // normal
		state->render_commands[0].shader = state->shader_normal_map;
	}
	if(xen::isKeyPressed(xen::Key::Num0, state->window)){ // phong
		state->render_commands[0].shader = state->shader_phong;
	}

	handleCameraInputCylinder(state->window, state->camera, xen::asSeconds<real>(cntx.dt), 30);
	state->render_params.camera = xen::generateCamera3d(state->camera);

	state->scene_lights[0].point.position = xen::rotated(Vec3r{0.5_r, 0.5_r, 0.0_r},
	                                                     Vec3r::UnitY,
	                                                     180_deg * xen::asSeconds<real>(cntx.time)
	                                                    );

  gmod->clear      (state->window, xen::Color{20, 20, 20, 255});
  gmod->render     (state->window, viewport, state->render_params, state->render_commands);
  gmod->swapBuffers(state->window);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

xen::Module exported_xen_module = {
	xen::hash("game"),
	&init,
	&shutdown,
	&load,
	&tick,
};
