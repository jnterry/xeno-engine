#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/glew.h>
#include <GL/gl.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>

#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/quaternion.hpp>

#include <xen/graphics/Image.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Light3d.hpp>

#include <xen/gl/Mesh.hxx>
#include <xen/gl/Shader.hxx>
#include <xen/gl/GlDevice.hpp>
#include <xen/gl/Texture.hpp>

#include "utilities.hpp"

xen::RenderParameters3d render_params;
xen::FixedArray<xen::LightSource3d, 1> light_sources;
xen::Camera3dCylinder camera;
real camera_speed = 50;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

static GLfloat cube_buffer_data[] = {
    -1.0f,-1.0f,-1.0f, // triangle 1 : begin
    -1.0f,-1.0f, 1.0f,
    -1.0f, 1.0f, 1.0f, // triangle 1 : end
     1.0f, 1.0f,-1.0f, // triangle 2 : begin
    -1.0f,-1.0f,-1.0f,
    -1.0f, 1.0f,-1.0f, // triangle 2 : end
     1.0f,-1.0f, 1.0f,
    -1.0f,-1.0f,-1.0f,
     1.0f,-1.0f,-1.0f,
     1.0f, 1.0f,-1.0f,
     1.0f,-1.0f,-1.0f,
    -1.0f,-1.0f,-1.0f,
    -1.0f,-1.0f,-1.0f,
    -1.0f, 1.0f, 1.0f,
    -1.0f, 1.0f,-1.0f,
     1.0f,-1.0f, 1.0f,
    -1.0f,-1.0f, 1.0f,
    -1.0f,-1.0f,-1.0f,
    -1.0f, 1.0f, 1.0f,
    -1.0f,-1.0f, 1.0f,
     1.0f,-1.0f, 1.0f,
     1.0f, 1.0f, 1.0f,
     1.0f,-1.0f,-1.0f,
     1.0f, 1.0f,-1.0f,
     1.0f,-1.0f,-1.0f,
     1.0f, 1.0f, 1.0f,
     1.0f,-1.0f, 1.0f,
     1.0f, 1.0f, 1.0f,
     1.0f, 1.0f,-1.0f,
    -1.0f, 1.0f,-1.0f,
     1.0f, 1.0f, 1.0f,
    -1.0f, 1.0f,-1.0f,
    -1.0f, 1.0f, 1.0f,
     1.0f, 1.0f, 1.0f,
    -1.0f, 1.0f, 1.0f,
     1.0f,-1.0f, 1.0f,


    // color
    0.0f, 0.0f, 0.0f, // Face A
    0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 1.0f, // Face B
    1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, // Face C
    1.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 1.0f, // Face B
    1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 0.0f, // Face A
    0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 0.0f, // Face C
    1.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, // Face E
    0.0f, 1.0f, 0.0f,
    0.0f, 1.0f, 0.0f,
    0.0f, 0.0f, 1.0f, // Face D
    0.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f, // Face D
    0.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 1.0f, // Face F
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f, // Face F
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 0.0f, // Face E
    0.0f, 1.0f, 0.0f,
    0.0f, 1.0f, 0.0f,

    // normals
    -1.0f, 0.0f, 0.0f, // triangle 1 : begin
    -1.0f, 0.0f, 0.0f,
    -1.0f, 0.0f, 0.0f, // triangle 1 : end
     0.0f, 0.0f,-1.0f, // triangle 2 : begin
     0.0f, 0.0f,-1.0f,
     0.0f, 0.0f,-1.0f, // triangle 2 : end
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
    -1.0f, 0.0f, 0.0f,
    -1.0f, 0.0f, 0.0f,
    -1.0f, 0.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f
};

int main(int argc, char** argv){
	camera.z_near = 0.001;
	camera.z_far  = 10000;
	camera.fov_y  = 80_deg;
	camera.radius = 100;
	camera.height = 0;
	camera.up_dir = Vec3r::UnitY;
	camera.axis   = Vec3r::UnitY;
	camera.angle  = 90_deg;

	render_params.lights = light_sources;

	sf::ContextSettings context_settings;
	context_settings.depthBits = 24;
	context_settings.stencilBits = 8;
	context_settings.antialiasingLevel = 4;
	context_settings.majorVersion = 3;
	context_settings.minorVersion = 0;

	xen::Aabb2u viewport = { 0, 0, 800, 600 };

	sf::Window app(sf::VideoMode(viewport.max.x, viewport.max.y, 32), "Window Title", sf::Style::Default, context_settings);

	context_settings = app.getSettings();
	printf("Initialized window, GL version: %i.%i\n", context_settings.majorVersion, context_settings.minorVersion);

	xen::Allocator* alloc  = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	xen::ArenaLinear arena = xen::createArenaLinear(*alloc, xen::megabytes(32));

	app.setActive(true);
	glewInit();

	Mat4r model_mat;
	Vec4r point_light_color = Vec4r(1,0,0,1);

	xen::GraphicsDevice* device = xen::createGlDevice(arena);

	xen::FixedArray<xen::VertexAttribute::Type, 3> vertex_spec;
	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Color3f;
	vertex_spec[2] = xen::VertexAttribute::Normal3r;

	xen::MeshData* mesh_data_bunny = xen::createEmptyMeshData(arena, vertex_spec);
	xen::loadMeshFile(mesh_data_bunny, arena, "bunny.obj");
	xen::gl::MeshHeader* mesh_bunny = xen::gl::createMesh(arena, *mesh_data_bunny);

	void* mesh_cube_attrib_data[] = {
		&cube_buffer_data[3*2*6 * 0 * 3],
		nullptr,//&cube_buffer_data[3*2*6 * 1 * 3],
		nullptr// &cube_buffer_data[3*2*6 * 2 * 3]
	};
  xen::MeshData mesh_data_cube;
	mesh_data_cube.attrib_count = 3;
	mesh_data_cube.attrib_types = vertex_spec.elements;
	mesh_data_cube.vertex_count = 3 * 2 * 6; // (3 vert per tri) * (2 tri per face) * (6 faces)
	mesh_data_cube.attrib_data  = mesh_cube_attrib_data;
	xen::gl::MeshHeader* mesh_cube = xen::gl::createMesh(arena, mesh_data_cube);

	xen::RawImage          test_image   = xen::loadImage(arena, "test.bmp");
	xen::gl::TextureHandle test_texture = xen::gl::createTexture(&test_image);

	sf::Clock timer;
	real last_time = 0;

	int CMD_BUNNY  = 0;
	int CMD_FLOOR  = 1;
	int CMD_LIGHT  = 2;
	int CMD_AXIS_X = 3;
	int CMD_AXIS_Y = 4;
	int CMD_AXIS_Z = 5;

	xen::FixedArray<xen::RenderCommand3d , 6> render_cmds;
	render_cmds[CMD_BUNNY ].type                = xen::RenderCommand3d::MESH;
	render_cmds[CMD_BUNNY ].color               = xen::Color::RED4f;
	render_cmds[CMD_BUNNY ].model_matrix        = Mat4r::Identity;
	render_cmds[CMD_BUNNY ].mesh_header         = mesh_bunny;

  render_cmds[CMD_FLOOR ].type                = xen::RenderCommand3d::MESH;
	render_cmds[CMD_FLOOR ].color               = xen::Color::WHITE4f;
	render_cmds[CMD_FLOOR ].model_matrix        = Mat4r::Identity;
	render_cmds[CMD_FLOOR ].mesh_header         = mesh_cube;

	render_cmds[CMD_LIGHT ].type                = xen::RenderCommand3d::MESH;
	render_cmds[CMD_LIGHT ].color               = xen::Color::RED4f;
	render_cmds[CMD_LIGHT ].emissive_color      = xen::Color::RED4f;
	render_cmds[CMD_LIGHT ].model_matrix        = Mat4r::Identity;
	render_cmds[CMD_LIGHT ].mesh_header         = mesh_cube;

	render_cmds[CMD_AXIS_X].type                = xen::RenderCommand3d::MESH;
	render_cmds[CMD_AXIS_X].color               = xen::Color::RED4f;
	render_cmds[CMD_AXIS_X].emissive_color      = xen::Color::RED4f;
	render_cmds[CMD_AXIS_X].model_matrix        = xen::Translation3d(1,0,0) * xen::Scale3d(5, 0.05, 0.05);
	render_cmds[CMD_AXIS_X].mesh_header         = mesh_cube;

	render_cmds[CMD_AXIS_Y].type                = xen::RenderCommand3d::MESH;
	render_cmds[CMD_AXIS_Y].color               = xen::Color::GREEN4f;
	render_cmds[CMD_AXIS_Y].emissive_color      = xen::Color::GREEN4f;
	render_cmds[CMD_AXIS_Y].model_matrix        = xen::Translation3d(0,1,0) * xen::Scale3d(0.05, 5, 0.05);
	render_cmds[CMD_AXIS_Y].mesh_header         = mesh_cube;

	render_cmds[CMD_AXIS_Z].type                = xen::RenderCommand3d::MESH;
	render_cmds[CMD_AXIS_Z].color               = xen::Color::BLUE4f;
	render_cmds[CMD_AXIS_Z].emissive_color      = xen::Color::BLUE4f;
	render_cmds[CMD_AXIS_Z].model_matrix        = xen::Translation3d(0,0,1) * xen::Scale3d(0.05, 0.05, 5);
	render_cmds[CMD_AXIS_Z].mesh_header         = mesh_cube;

	printf("Entering main loop\n");
	while(app.isOpen()){
		float time = timer.getElapsedTime().asSeconds();
		real dt = time - last_time;
		last_time = time;

		sf::Event event;
		while(app.pollEvent(event)){
			switch(event.type){
			case sf::Event::Closed:
				app.close();
				break;
			case sf::Event::Resized:
				XEN_CHECK_GL(glViewport(0,0,event.size.width, event.size.height));
				viewport.max = {event.size.width, event.size.height};
				break;
			case sf::Event::KeyReleased:
				switch(event.key.code){
				case sf::Keyboard::R:
					point_light_color.xyz = Vec3r(1,0,0);
					break;
				case sf::Keyboard::G:
					point_light_color.xyz = Vec3r(0,1,0);
					break;
				case sf::Keyboard::B:
					point_light_color.xyz = Vec3r(0,0,1);
					break;
				case sf::Keyboard::W:
					point_light_color.xyz = Vec3r(1,1,1);
					break;
				default: break;
				}
				break;
			default: break;
			}
		}

		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Up)){
			camera.radius -= camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Down)){
			camera.radius += camera_speed * dt;
		}
		camera.radius = xen::clamp(camera.radius, 0.01_r, 100_r);
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Left)){
			camera.angle -= camera_rotate_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Right)){
			camera.angle += camera_rotate_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::A)){
			camera.height += camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Z)){
			camera.height -= camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Q)){
			camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::E)){
			camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
		}

		app.setActive(true);

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
		model_mat *= xen::Translation3d(-0.5_r * (mesh_bunny->bounds.max - mesh_bunny->bounds.min));
		model_mat *= xen::Rotation3dy(67_deg * time);
		render_cmds[CMD_BUNNY].model_matrix = model_mat;

		////////////////////////////////////////////
		// Draw Floor
		model_mat = Mat4r::Identity;
		model_mat *= xen::Scale3d(30, 0.05, 30);
		model_mat *= xen::Translation3d(0, -0.5, 0);
		render_cmds[CMD_FLOOR].model_matrix = model_mat;

		device->clear (xen::makeNullHandle<xen::RenderTarget>(), viewport,
		               xen::Color::BLACK
		              );
		device->render(xen::makeNullHandle<xen::RenderTarget>(), viewport,
		               render_params, render_cmds
		              );
		app.display();
	}
	printf("Exiting main loop\n");

	xen::destroyArenaLinear(*alloc, arena);

	return 0;
}
