#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/glew.h>
#include <GL/gl.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/util/File.hpp>
#include <xen/gl/Shader.hpp>
#include <xen/gl/Mesh.hpp>
#include <xen/gl/gl_header.hxx>
#include <xen/gl/Texture.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/quaternion.hpp>

#include "utilities.hpp"

void renderMesh(const xen::gl::Mesh* mesh);
xen::gl::ShaderProgram* loadShader(xen::ArenaLinear&);

xen::Camera3dCylinder camera;
real camera_speed = 50;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

static const GLfloat cube_buffer_data[] = {
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
	camera.angle    = 90_deg;

	sf::ContextSettings context_settings;
	context_settings.depthBits = 24;
	context_settings.stencilBits = 8;
	context_settings.antialiasingLevel = 4;
	context_settings.majorVersion = 3;
	context_settings.minorVersion = 0;

	Vec2r window_size = {800, 600};

	sf::Window app(sf::VideoMode(window_size.x, window_size.y, 32), "Window Title", sf::Style::Default, context_settings);

	context_settings = app.getSettings();
	printf("Initialized window, GL version: %i.%i\n", context_settings.majorVersion, context_settings.minorVersion);

	XenTempArena(arena, 8196);

	app.setActive(true);
	glewInit();

	XEN_CHECK_GL(glEnable(GL_DEPTH_TEST));
	XEN_CHECK_GL(glDepthFunc(GL_LESS));

	Mat4r model_mat, view_mat, proj_mat, vp_mat;
	Vec4r point_light_color = Vec4r(1,0,0,1);

	xen::gl::ShaderProgram* prog  = loadShader(arena);
	int mvp_mat_loc           = xen::gl::getUniformLocation(prog, "mvp_mat"          );
	int model_mat_loc         = xen::gl::getUniformLocation(prog, "model_mat"        );
	int point_light_pos_loc   = xen::gl::getUniformLocation(prog, "point_light_pos"  );
	int point_light_color_loc = xen::gl::getUniformLocation(prog, "point_light_color");
	int emissive_color_loc    = xen::gl::getUniformLocation(prog, "emissive_color"   );
	int camera_pos_loc        = xen::gl::getUniformLocation(prog, "camera_position"  );

	xen::VertexAttributeType vertex_spec[] = {
		{ xen::VertexAttributeType::PositionXYZ },
		{ xen::VertexAttributeType::ColorRGBf   },
		{ xen::VertexAttributeType::NormalXYZ   }
	};

	xen::gl::Mesh* mesh_bunny = xen::gl::loadMesh(arena, "bunny.obj");

	const void* cube_attrib_data[] = {&cube_buffer_data[3*2*6 * 0 * 3],
	                                  nullptr,//&cube_buffer_data[3*2*6 * 1 * 3],
	                                  nullptr// &cube_buffer_data[3*2*6 * 2 * 3]
	};
	XenAssert(XenArrayLength(vertex_spec) == XenArrayLength(cube_attrib_data),
	          "Vertex spec attrib count must match num attribs used");
	xen::gl::Mesh* mesh_cube = xen::gl::createMesh
		(
		 arena,
		 XenArrayLength(vertex_spec), vertex_spec,
		 cube_attrib_data,
		 3 * 2 * 6 // Vertex count: (3 vert per tri) * (2 tri per face) * (6 faces)
		);

	xen::RawImage          test_image   = xen::loadImage(arena, "test.bmp");
	xen::gl::TextureHandle test_texture = xen::gl::createTexture(&test_image);

	sf::Clock timer;
	real last_time = 0;

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
				window_size = {(real)event.size.width, (real)event.size.height};
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

		view_mat = getViewMatrix(camera);
		proj_mat = getProjectionMatrix(camera, window_size);
		vp_mat   = view_mat * proj_mat;

		app.setActive(true);
		XEN_CHECK_GL(glClearColor(0.1,0.1,0.1, 1));
		XEN_CHECK_GL(glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT));

		xen::gl::useShader(prog);

		Vec3r light_pos = xen::rotated(Vec3r{4, 3, 0}, Vec3r::UnitY, xen::Degrees(time*90_r));
		xen::gl::setUniform(point_light_pos_loc, light_pos);
		point_light_color.w = (1_r + sin(time*9)) / 2.0_r;
		xen::gl::setUniform(point_light_color_loc, point_light_color);
		xen::gl::setUniform(camera_pos_loc, getCameraPosition(camera));

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
		xen::gl::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::gl::setUniform(model_mat_loc, model_mat);
		xen::gl::setUniform(emissive_color_loc, point_light_color);
		renderMesh(mesh_cube);
		xen::gl::setUniform(emissive_color_loc, Vec4r::Origin);
		////////////////////////////////////////////

		////////////////////////////////////////////
		// Draw Bunny
		model_mat = Mat4r::Identity;
		//model_mat *= xen::Rotation3dz(73_deg * time);
		model_mat *= xen::Scale3d(20);
		model_mat *= xen::Translation3d(-0.5_r * (mesh_bunny->bounds_max - mesh_bunny->bounds_min));
		model_mat *= xen::Rotation3dy(67_deg * time);
		//model_mat *= xen::Translation3d(0, 0, 0);
		xen::gl::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::gl::setUniform(model_mat_loc, model_mat);
		renderMesh(mesh_bunny);
		////////////////////////////////////////////

		////////////////////////////////////////////
		// Draw Floor
		model_mat = Mat4r::Identity;
		model_mat *= xen::Scale3d(30, 0.05, 30);
		model_mat *= xen::Translation3d(0, -0.5, 0);
		xen::gl::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::gl::setUniform(model_mat_loc, model_mat);
		renderMesh(mesh_cube);
		////////////////////////////////////////////

		////////////////////////////////////////////
		// Draw Axes
		model_mat = Mat4r::Identity;
		model_mat *= xen::Translation3d(1,0,0);
		model_mat *= xen::Scale3d(5, 0.05, 0.05);
		xen::gl::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::gl::setUniform(model_mat_loc, model_mat);
		xen::gl::setUniform(emissive_color_loc, Vec4r(1,0,0,1));
		renderMesh(mesh_cube);

		model_mat = Mat4r::Identity;
		model_mat *= xen::Translation3d(0,1,0);
		model_mat *= xen::Scale3d(0.05, 5, 0.05);
		xen::gl::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::gl::setUniform(model_mat_loc, model_mat);
		xen::gl::setUniform(emissive_color_loc, Vec4r(0,1,0,1));
		renderMesh(mesh_cube);

		model_mat = Mat4r::Identity;
		model_mat *= xen::Translation3d(0,0,1);
		model_mat *= xen::Scale3d(0.05, 0.05, 5);
		xen::gl::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::gl::setUniform(model_mat_loc, model_mat);
		xen::gl::setUniform(emissive_color_loc, Vec4r(0,0,1,1));
		renderMesh(mesh_cube);
		////////////////////////////////////////////

		app.display();
	}
	printf("Exiting main loop\n");

	return 0;
}

void renderMesh(const xen::gl::Mesh* mesh){
	XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, mesh->gpu_buffer->handle));

	for(int i = 0; i < mesh->attribute_count; ++i){
		if(mesh->attribute_sources[i].buffer == nullptr){
			XEN_CHECK_GL(glDisableVertexAttribArray(i));
			// :TODO: this relies on real being a float
			XEN_CHECK_GL(glVertexAttrib3f(i,
			                              mesh->attribute_sources[i].vec3r.x,
			                              mesh->attribute_sources[i].vec3r.y,
			                              mesh->attribute_sources[i].vec3r.z
			                             )
			             );
		} else {
			XEN_CHECK_GL(glEnableVertexAttribArray(i));
			XEN_CHECK_GL(glVertexAttribPointer(i,           //attrib layout
			                                   3, GL_FLOAT, // num components and type
			                                   GL_FALSE,    // normalised
			                                   mesh->attribute_sources[i].stride,
			                                   (void*)mesh->attribute_sources[i].offset
			                                  )
			             );
		}
	}

	XEN_CHECK_GL(glDrawArrays(GL_TRIANGLES, 0, mesh->num_triangles * 3));
}

xen::gl::ShaderProgram* loadShader(xen::ArenaLinear& arena){
	XenTempArena(scratch, 8196);

	xen::FileData vertex_src = xen::loadFileAndNullTerminate(scratch, "vertex.glsl");
	xen::FileData pixel_src  = xen::loadFileAndNullTerminate(scratch, "pixel.glsl");

	auto result = xen::gl::createShaderProgram(arena, (char*)vertex_src.data, (char*)pixel_src.data);

	if(!xen::gl::isOkay(result)){
		xen::resetArena(scratch);
		const char* errors = xen::gl::getErrors(result, scratch);
		printf("Shader Errors:\n%s\n", errors);
		exit(1);
	} else {
		printf("Shader compiled successfully\n");
	}

	return result;
}
