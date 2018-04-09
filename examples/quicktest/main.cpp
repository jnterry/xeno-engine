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
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Light3d.hpp>
#include <xen/graphics/Window.hpp>

#include <xen/gl/gl_header.hxx>
#include <xen/gl/GlDevice.hpp>
#include <xen/gl/Texture.hpp>

#include "../common.cpp"

xen::RenderParameters3d render_params;
xen::FixedArray<xen::LightSource3d, 1> light_sources;
xen::Camera3dCylinder camera;

int main(int argc, char** argv){
	camera.z_near = 0.001;
	camera.z_far  = 10000;
	camera.fov_y  = 80_deg;
	camera.radius = 100;
	camera.height = 0;
	camera.up_dir = Vec3r::UnitY;
	camera.axis   = Vec3r::UnitY;
	camera.angle  = 0_deg;

	render_params.lights = light_sources;

	xen::Allocator* alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear arena = xen::createArenaLinear(*alloc, xen::megabytes(32));

	printf("Initialized main arena\n");

	xen::GraphicsDevice* device = xen::createGlDevice(arena);

	printf("Created gl device\n");

	xen::Window* app = device->createWindow({800, 600}, "Quicktest");

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app) };

	Mat4r model_mat;
	xen::Color4f point_light_color = xen::Color4f(1,0,0,1);

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Color3f;
	vertex_spec[2] = xen::VertexAttribute::Normal3r;

	xen::MeshData* mesh_data_bunny = xen::createEmptyMeshData(arena, vertex_spec);
	xen::loadMeshFile(mesh_data_bunny, arena, "bunny.obj");
	xen::Mesh mesh_bunny = device->createMesh(*mesh_data_bunny);

	void* mesh_cube_attrib_data[] = {
		xen::TestMeshGeometry_UnitCube.position,
		nullptr, // xen::TestMeshGeometry_UnitCube.normal,
		nullptr, // &xen::TestMeshGeometry_UnitCube.normal,
	};
  xen::MeshData mesh_data_cube;
	mesh_data_cube.attrib_count = 3;
	mesh_data_cube.attrib_types = vertex_spec.elements;
	mesh_data_cube.vertex_count = 3 * 2 * 6; // (3 vert per tri) * (2 tri per face) * (6 faces)
	mesh_data_cube.attrib_data  = mesh_cube_attrib_data;
	xen::Mesh mesh_cube = device->createMesh(mesh_data_cube);

	xen::RawImage          test_image   = xen::loadImage(arena, "test.bmp");
	xen::gl::createTexture(&test_image);

	int CMD_BUNNY  = 0;
	int CMD_FLOOR  = 1;
	int CMD_LIGHT  = 2;
	int CMD_AXIS_X = 3;
	int CMD_AXIS_Y = 4;
	int CMD_AXIS_Z = 5;

	xen::FixedArray<xen::RenderCommand3d , 6> render_cmds;
	xen::clearToZero(render_cmds);

	render_cmds[CMD_BUNNY ].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_cmds[CMD_BUNNY ].color           = xen::Color::RED4f;
	render_cmds[CMD_BUNNY ].model_matrix    = Mat4r::Identity;
	render_cmds[CMD_BUNNY ].geometry_source = xen::RenderCommand3d::MESH;
	render_cmds[CMD_BUNNY ].mesh            = mesh_bunny;

	model_mat = Mat4r::Identity;

		model_mat *= xen::Translation3d(0, -0.5, 0);
		render_cmds[CMD_FLOOR].model_matrix = model_mat;

	render_cmds[CMD_FLOOR ].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_cmds[CMD_FLOOR ].color           = xen::Color::WHITE4f;
	render_cmds[CMD_FLOOR ].model_matrix    = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                           xen::Scale3d(60, 0.1, 60) *
	                                           xen::Translation3d(0, -0.5_r, 0)
	                                          );
	render_cmds[CMD_FLOOR ].geometry_source = xen::RenderCommand3d::MESH;
	render_cmds[CMD_FLOOR ].mesh            = mesh_cube;

	render_cmds[CMD_LIGHT ].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_cmds[CMD_LIGHT ].color           = xen::Color::RED4f;
	render_cmds[CMD_LIGHT ].emissive_color  = xen::Color::RED4f;
	render_cmds[CMD_LIGHT ].model_matrix    = Mat4r::Identity;
	render_cmds[CMD_LIGHT ].geometry_source = xen::RenderCommand3d::MESH;
	render_cmds[CMD_LIGHT ].mesh            = mesh_cube;

	render_cmds[CMD_AXIS_X].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_cmds[CMD_AXIS_X].color           = xen::Color::RED4f;
	render_cmds[CMD_AXIS_X].emissive_color  = xen::Color::RED4f;
	render_cmds[CMD_AXIS_X].model_matrix    = xen::Scale3d(15, 0.1, 0.1);
	render_cmds[CMD_AXIS_X].geometry_source = xen::RenderCommand3d::MESH;
	render_cmds[CMD_AXIS_X].mesh            = mesh_cube;

	render_cmds[CMD_AXIS_Y].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_cmds[CMD_AXIS_Y].color           = xen::Color::GREEN4f;
	render_cmds[CMD_AXIS_Y].emissive_color  = xen::Color::GREEN4f;
	render_cmds[CMD_AXIS_Y].model_matrix    = xen::Scale3d(0.1, 15, 0.1);
	render_cmds[CMD_AXIS_Y].geometry_source = xen::RenderCommand3d::MESH;
	render_cmds[CMD_AXIS_Y].mesh            = mesh_cube;

	render_cmds[CMD_AXIS_Z].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_cmds[CMD_AXIS_Z].color           = xen::Color::BLUE4f;
	render_cmds[CMD_AXIS_Z].emissive_color  = xen::Color::BLUE4f;
	render_cmds[CMD_AXIS_Z].model_matrix    = xen::Scale3d(0.1, 0.1, 15);
	render_cmds[CMD_AXIS_Z].geometry_source = xen::RenderCommand3d::MESH;
	render_cmds[CMD_AXIS_Z].mesh            = mesh_cube;

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");
	while(xen::isWindowOpen(app)){
	  real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;
		printf("dt: %f\n", dt);

		xen::WindowEvent* event;
		while((event = xen::pollEvent(app)) != nullptr){
			switch(event->type){
			case xen::WindowEvent::Closed:
				device->destroyWindow(app);
				break;
			case xen::WindowEvent::Resized:
				viewport.max = event->resize.new_size;
				break;
			case xen::WindowEvent::KeyReleased:
				switch(event->key.key){
				case xen::Key::R:
					point_light_color.rgb = xen::Color3f(1,0,0);
					break;
				case xen::Key::G:
					point_light_color.rgb = xen::Color3f(0,1,0);
					break;
				case xen::Key::B:
					point_light_color.rgb = xen::Color3f(0,0,1);
					break;
				case xen::Key::W:
					point_light_color.rgb = xen::Color3f(1,1,1);
					break;
				default: break;
				}
				break;
			default: break;
			}
		}
		handleCameraInputCylinder(camera, dt);

		Vec3r light_pos = xen::rotated(Vec3r{4, 3, 0}, Vec3r::UnitY, xen::Degrees(time*90_r));
		point_light_color.w = (1_r + sin(time*9)) / 2.0_r;

		////////////////////////////////////////////
		// Set render params
		render_params.camera = xen::generateCamera3d(camera);
		render_params.lights[0].point.position = light_pos;
		render_params.lights[0].color          = point_light_color;

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
		render_cmds[CMD_LIGHT].model_matrix   = model_mat;
		render_cmds[CMD_LIGHT].emissive_color = point_light_color;

		////////////////////////////////////////////
		// Draw Bunny
		model_mat = Mat4r::Identity;
		//model_mat *= xen::Rotation3dz(73_deg * time);
		model_mat *= xen::Scale3d(20);
		model_mat *= xen::Translation3d(-0.5_r * (mesh_data_bunny->bounds.max - mesh_data_bunny->bounds.min));
		model_mat *= xen::Rotation3dy(67_deg * time);
		render_cmds[CMD_BUNNY].model_matrix = model_mat;

		////////////////////////////////////////////
		// Do rendering
		device->clear (app, xen::Color::BLACK);
		device->render(app, viewport, render_params, render_cmds);
		device->swapBuffers(app);
	}
	printf("Exiting main loop\n");

	xen::destroyArenaLinear(*alloc, arena);
	delete alloc;

	return 0;
}
