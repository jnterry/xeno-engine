#include <stdio.h>

#include "../utilities.hpp"
#include "mesh.hpp"
#include <xen/graphics/Image.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

// Locations for the boxes in the cornell scene
const Vec3r tall_box_center  = {-0.15_r,  0.0_r, -0.10_r};
const Vec3r short_box_center = { 0.18_r,  0.0_r,  0.18_r};

struct State {
	xen::ArenaLinear                       arena;
	xen::Camera3dCylinder                  camera;
	xen::RenderParameters3d                render_params;
	xen::FixedArray<xen::LightSource3d, 2> scene_lights;

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
	xen::Mesh                                      mesh_cornell_walls;
	xen::Mesh                                      mesh_cube;
	xen::Mesh                                      mesh_axes;

	const xen::Material* material;

	xen::Window* window;
	xen::RenderTarget window_target;

	xen::FixedArray<xen::RenderCommand3d, 5> render_cmds;
};

State* state = nullptr;

void initRenderCommands(xen::ModuleApiGraphics* mod_ren, xen::ArenaLinear& arena){
	state->material = mod_ren->createMaterial(material_creation_params_phong);

	xen::clearToZero(state->render_cmds);

	for(u64 i = 0; i < state->render_cmds.size; ++i){
		state->render_cmds[i].material = state->material;
		state->render_cmds[i].material_params = xen::reserveBytes(
			arena, state->material->parameters->size
		);
		xen::setMaterialParam(state->render_cmds[i], "emissive_color", xen::Color::BLACK4f);
		xen::setMaterialParam(state->render_cmds[i], "diffuse_color",  xen::Color::WHITE4f);
	}


	state->render_cmds[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[0].model_matrix    = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
	                                             xen::Rotation3dy(180_deg)
	                                            );
	state->render_cmds[0].mesh            = state->mesh_cornell_walls;
	// There is nothing outside of the cornell box, don't cast shadows for speed
	state->render_cmds[0].disable_shadow_cast = true;

	state->render_cmds[1].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[1].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r) *
	                                             xen::Scale3d      (0.3_r, 0.6_r, 0.3_r  ) *
	                                             xen::Rotation3dy  (15_deg               ) *
	                                             xen::Translation3d(tall_box_center      )
	                                            );
	state->render_cmds[1].mesh            = state->mesh_cube;
	xen::setMaterialParam(state->render_cmds[1], "diffuse_color", Vec4f{ 0.15f, 0.15f, 0.75f, 1.0f });


	state->render_cmds[2].primitive_type  = xen::PrimitiveType::TRIANGLES;
  state->render_cmds[2].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r) *
                                               xen::Scale3d      (0.3_r, 0.3_r, 0.3_r  ) *
                                               xen::Rotation3dy  (-18_deg              ) *
                                               xen::Translation3d(short_box_center     )
                                              );
	state->render_cmds[2].mesh            = state->mesh_cube;
	xen::setMaterialParam(state->render_cmds[2], "diffuse_color", Vec4f{ 0.75f, 0.15f, 0.15f, 1.0_r });

	state->render_cmds[3].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[3].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r  ) *
	                                             xen::Scale3d      (0.05_r, 0.05_r, 0.05_r ) *
	                                             xen::Rotation3dy  (18_deg                 ) *
	                                             xen::Translation3d(-0.1_r, 0.0_r, 0.2_r    )
	                                            );
	state->render_cmds[3].mesh            = state->mesh_cube;
	xen::setMaterialParam(state->render_cmds[3], "diffuse_color",  xen::Color::YELLOW4f);
	xen::setMaterialParam(state->render_cmds[3], "emissive_color", xen::Color::YELLOW4f);

	// Light source
	state->render_cmds[4].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_cmds[4].model_matrix    = Mat4r::Identity;
	state->render_cmds[4].mesh            = state->mesh_cube;
  // This is geometry around a light source - don't block the emitted light!
	state->render_cmds[4].disable_shadow_cast = true;
	xen::setMaterialParam(state->render_cmds[4], "diffuse_color",  xen::Color::RED4f);
	xen::setMaterialParam(state->render_cmds[4], "emissive_color", xen::Color::RED4f);
}

void initCamera(){
	state->camera.z_near = 0.001;
	state->camera.z_far  = 1000;
	state->camera.fov_y  = 70_deg;
	state->camera.radius = 10;
	state->camera.height = 0.001;
	state->camera.up_dir = Vec3r::UnitY;
	state->camera.axis   = Vec3r::UnitY;
	state->camera.target = {0.0_r, 0.5_r, 0.0_r};
	state->camera.angle  = 0.0_deg;
}

void initSceneLights(){
	state->scene_lights[0].type           = xen::LightSource3d::POINT;
	state->scene_lights[0].point.position = Vec3r{0.0_r, 0.99_r, 0.0_r};
	state->scene_lights[0].color          = xen::Color::WHITE4f;
	state->scene_lights[0].color.a        = 0.4f;
	state->scene_lights[0].attenuation    = {0.0f, 0.0f, 2.0f};

	state->scene_lights[1].type           = xen::LightSource3d::POINT;
	state->scene_lights[1].color          = xen::Color::RED4f;
	state->scene_lights[1].color.a        = 0.1f;
	state->scene_lights[1].attenuation    = {0.0f, 0.0f, 2.0f};

	state->render_params.ambient_light = xen::Color3f(0.1f, 0.1f, 0.1f);
	state->render_params.lights        = state->scene_lights;
}

void initMeshes(xen::ModuleApiGraphics* mod_ren){
	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;

	state->mesh_cornell_walls = mod_ren->createMesh(
		state->vertex_spec, MeshGeometry_CornellBoxWalls
	);
	state->mesh_cube = mod_ren->createMesh(
		state->vertex_spec, xen::TestMeshGeometry_UnitCube
	);
	state->mesh_axes = mod_ren->createMesh(
		state->vertex_spec, xen::TestMeshGeometry_Axes
	);
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

	state->window        = mod_win->createWindow({600, 600}, "cornell-box");
	state->window_target = mod_ren->createWindowRenderTarget(state->window);

	initCamera();
	initSceneLights();
	initMeshes(mod_ren);
	initRenderCommands(mod_ren, state->arena);

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
	handleCameraInputCylinder(mod_win, state->window, state->camera, xen::asSeconds<real>(cntx.dt), 20);
	state->render_params.camera = xen::generateCamera3d(state->camera);

	Vec3r light_1_pos = (tall_box_center +
	                     xen::rotated(Vec3r{0.3_r, 0.5_r, 0.0_r},
	                                  Vec3r::UnitY,
	                                  90_deg * xen::asSeconds<real>(cntx.time)
	                                 )
	                    );
	state->render_cmds[4].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                          xen::Scale3d(0.03_r) *
	                                          xen::Translation3d(light_1_pos)
	                                         );
	state->scene_lights[1].point.position = light_1_pos;

  mod_ren->clear      (state->window_target, xen::Color{20, 20, 20, 255});
  mod_ren->render     (state->window_target, viewport, state->render_params, state->render_cmds);
  mod_ren->swapBuffers(state->window_target);

}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick)
