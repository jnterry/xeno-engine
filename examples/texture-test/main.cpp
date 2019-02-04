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

	xen::Window*      window;
	xen::RenderTarget window_target;

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

void initMeshes(xen::ModuleApiGraphics* mod_ren){
	xen::ArenaLinear& arena = xen::getThreadScratchSpace();

	xen::MemoryTransaction transaction(arena);

	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;
	state->vertex_spec[3] = xen::VertexAttribute::TexCoord2f;

	state->mesh_xzplane = mod_ren->createMesh(state->vertex_spec,
	                                       xen::TestMeshGeometry_UnitXzPlaneCentered
	                                      );

	xen::RawImage image_bricks_diffuse = xen::loadImage(arena, "resource/texture/bricks-diffuse.jpg");
	xen::RawImage image_bricks_normal  = xen::loadImage(arena, "resource/texture/bricks-normal.jpg");
	xen::RawImage image_metal_diffuse  = xen::loadImage(arena, "resource/texture/metal-plate-diffuse.jpg");
	xen::RawImage image_metal_normal   = xen::loadImage(arena, "resource/texture/metal-plate-normal.jpg");

	state->texture_bricks_diffuse = mod_ren->createTexture(&image_bricks_diffuse);
	state->texture_bricks_normal  = mod_ren->createTexture(&image_bricks_normal);
	state->texture_metal_diffuse  = mod_ren->createTexture(&image_metal_diffuse);
	state->texture_metal_normal   = mod_ren->createTexture(&image_metal_normal);

	state->shader_normal_map = mod_ren->createShader({ (void*)&FragmentShader_NormalMap });
	state->shader_phong      = mod_ren->createShader({ (void*)&FragmentShader_Phong     });
}

void* init(const void* params){
	xen::ModuleApiGraphics* mod_ren = xen::getModuleApi<xen::ModuleApiGraphics>();
	xen::ModuleApiWindow*   mod_win = xen::getModuleApi<xen::ModuleApiWindow>();
	XenAssert(mod_ren != nullptr, "Graphics module must be loaded before cornell-box");
	XenAssert(mod_win != nullptr, "Window module must be loaded before cornell-box");

	state = (State*)xen::kernelAlloc(sizeof(State));

	state->window        = mod_win->createWindow({800, 600}, "texture-test");
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

	if(mod_win->isKeyPressed(xen::Key::Num1)){ // bricks
		state->render_commands[0].textures[0]        = state->texture_bricks_diffuse;
		state->render_commands[0].textures[1]        = state->texture_bricks_normal;
		state->render_commands[0].specular_exponent  = 5_r;
		state->render_commands[0].specular_intensity = 0.5_r;
	}
	if(mod_win->isKeyPressed(xen::Key::Num2)){ // metal
		state->render_commands[0].textures[0]        = state->texture_metal_diffuse;
		state->render_commands[0].textures[1]        = state->texture_metal_normal;
		state->render_commands[0].specular_exponent  = 100_r;
		state->render_commands[0].specular_intensity = 5_r;
	}

	if(mod_win->isKeyPressed(xen::Key::Num9)){ // normal
		state->render_commands[0].shader = state->shader_normal_map;
	}
	if(mod_win->isKeyPressed(xen::Key::Num0)){ // phong
		state->render_commands[0].shader = state->shader_phong;
	}

	handleCameraInputCylinder(mod_win, state->window, state->camera, xen::asSeconds<real>(cntx.dt), 30);
	state->render_params.camera = xen::generateCamera3d(state->camera);

	state->scene_lights[0].point.position = xen::rotated(Vec3r{0.5_r, 0.5_r, 0.0_r},
	                                                     Vec3r::UnitY,
	                                                     180_deg * xen::asSeconds<real>(cntx.time)
	                                                    );

  mod_ren->clear      (state->window_target, xen::Color{20, 20, 20, 255});
  mod_ren->render     (state->window_target, viewport, state->render_params, state->render_commands);
  mod_ren->swapBuffers(state->window_target);
}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
