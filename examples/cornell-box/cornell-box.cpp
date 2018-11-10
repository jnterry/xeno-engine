#include <stdio.h>

#include "../utilities.hpp"
#include "mesh.hpp"
#include <xen/graphics/Image.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/math/quaternion.hpp>

// Locations for the boxes in the cornell scene
const Vec3r tall_box_center  = {-0.15_r,  0.0_r, -0.10_r};
const Vec3r short_box_center = { 0.18_r,  0.0_r,  0.18_r};

struct State {
	xen::Camera3dCylinder                  camera;
	xen::RenderParameters3d                render_params;
	xen::FixedArray<xen::LightSource3d, 2> scene_lights;

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
	xen::Mesh                                      mesh_cornell_walls;
	xen::Mesh                                      mesh_cube;
	xen::Mesh                                      mesh_axes;

	xen::Window* window;

	xen::FixedArray<xen::RenderCommand3d, 5> render_commands;
};

State* state = nullptr;

void initRenderCommands(){
	xen::clearToZero(state->render_commands);

	state->render_commands[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_commands[0].color           = xen::Color::WHITE4f;
	state->render_commands[0].model_matrix    = (xen::Translation3d(-0.5_r, 0.0_r, -0.5_r) *
	                                             xen::Rotation3dy(180_deg)
	                                            );
	state->render_commands[0].mesh            = state->mesh_cornell_walls;
	// There is nothing outside of the cornell box, don't cast shadows for speed
	state->render_commands[0].flags           = xen::RenderCommand3d::Flags::DisableShadowCast;

	state->render_commands[1].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_commands[1].color           = Vec4f{ 0.15f, 0.15f, 0.75f, 1.0f };
	state->render_commands[1].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r) *
	                                             xen::Scale3d      (0.3_r, 0.6_r, 0.3_r  ) *
	                                             xen::Rotation3dy  (15_deg               ) *
	                                             xen::Translation3d(tall_box_center      )
	                                            );
	state->render_commands[1].mesh            = state->mesh_cube;

	state->render_commands[2].primitive_type  = xen::PrimitiveType::TRIANGLES;
  state->render_commands[2].color           = Vec4f{ 0.75f, 0.15f, 0.15f, 1.0_r };
  state->render_commands[2].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r) *
                                               xen::Scale3d      (0.3_r, 0.3_r, 0.3_r  ) *
                                               xen::Rotation3dy  (-18_deg              ) *
                                               xen::Translation3d(short_box_center     )
                                              );
	state->render_commands[2].mesh            = state->mesh_cube;

	state->render_commands[3].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_commands[3].color           = xen::Color::YELLOW4f;
	state->render_commands[3].emissive_color  = xen::Color::YELLOW4f;
	state->render_commands[3].model_matrix    = (xen::Translation3d(-0.5_r, 0.0001_r, -0.5_r  ) *
	                                             xen::Scale3d      (0.05_r, 0.05_r, 0.05_r ) *
	                                             xen::Rotation3dy  (18_deg                 ) *
	                                             xen::Translation3d(-0.1_r, 0.0_r, 0.2_r    )
	                                            );
	state->render_commands[3].mesh            = state->mesh_cube;

	// Light source
	state->render_commands[4].primitive_type  = xen::PrimitiveType::TRIANGLES;
	state->render_commands[4].color           = xen::Color::RED4f;
	state->render_commands[4].model_matrix    = Mat4r::Identity;
	state->render_commands[4].mesh            = state->mesh_cube;
  // This is geometry around a light source - don't block the emitted light!
	state->render_commands[4].flags           = xen::RenderCommand3d::Flags::DisableShadowCast;
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
	state->scene_lights[0].point.position = Vec3r{0.0_r, 1.0_r, 0.0_r};
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

void initMeshes(xen::GraphicsModuleApi* gmod){
	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Normal3r;
	state->vertex_spec[2] = xen::VertexAttribute::Color4b;

	state->mesh_cornell_walls = gmod->createMesh(state->vertex_spec,
	                                             MeshGeometry_CornellBoxWalls
	                                            );
	state->mesh_cube = gmod->createMesh(state->vertex_spec,
	                                    xen::TestMeshGeometry_UnitCube
	                                   );
	state->mesh_axes = gmod->createMesh(state->vertex_spec,
	                                    xen::TestMeshGeometry_Axes
	                                   );
}

void* init( const void* params){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr, "Graphics module must be loaded before cornell-box");

	state = (State*)xen::kernelAlloc(sizeof(State));

	state->window = gmod->createWindow({600, 600}, "cornell-box");

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
	XenAssert(gmod != nullptr, "Graphics module must be loaded before cornell-box");

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(state->window) };

	xen::WindowEvent* event;
	while((event = xen::pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
			gmod->destroyWindow(state->window);
			break;
		default: break;
		}
	}
	handleCameraInputCylinder(state->window, state->camera, xen::asSeconds<real>(cntx.dt), 20);
	state->render_params.camera = xen::generateCamera3d(state->camera);

	Vec3r light_1_pos = (tall_box_center +
	                     xen::rotated(Vec3r{0.3_r, 0.5_r, 0.0_r},
	                                  Vec3r::UnitY,
	                                  90_deg * xen::asSeconds<real>(cntx.time)
	                                 )
	                    );
	state->render_commands[4].model_matrix = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                          xen::Scale3d(0.03_r) *
	                                          xen::Translation3d(light_1_pos)
	                                         );
	state->scene_lights[1].point.position = light_1_pos;

  gmod->clear      (state->window, xen::Color{20, 20, 20, 255});
  gmod->render     (state->window, viewport, state->render_params, state->render_commands);
  gmod->swapBuffers(state->window);

}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

XenDeclareModule("game", &init, &shutdown, &load, nullptr, &tick);
