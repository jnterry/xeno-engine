#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/glew.h>
#include <GL/gl.h>

#include "xen/core/intrinsics.hpp"
#include "xen/core/memory.hpp"
#include "xen/graphics/Shader.hpp"
#include "xen/math/Vector.hpp"
#include "xen/math/Matrix.hpp"
#include "xen/math/Angle.hpp"

#include "utilities.hpp"
#include "Camera3d.hpp"

void initCube();
void renderCube();

Camera3d camera;
real camera_speed = 10;
xen::Angle camera_angle = 0_deg;
real camera_radius = 10;
real camera_height = 0;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

xen::ShaderProgram* loadShader(xen::ArenaLinear&);

int main(int argc, char** argv){

	xen::AllocatorCounter<xen::AllocatorMalloc> alloc;

	camera.look_dir = Vec3r::UnitZ;
	camera.z_near   = 0.001;
	camera.z_far    = 10000;
	camera.fov_y    = 80_deg;

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

	XenTempArena(arena, 4096);

	app.setActive(true);
	glewInit();
	initCube();

	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);

	Mat4r view_mat;

	xen::ShaderProgram* prog = loadShader(arena);
	int mvpMatLoc = xen::getUniformLocation(prog, "mvpMatrix");
	Mat4r model_mat;

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
				glViewport(0,0,event.size.width, event.size.height);
				window_size = {(real)event.size.width, (real)event.size.height};
				break;
			default: break;
			}
		}

		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Up)){
			camera_radius -= camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Down)){
			camera_radius += camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Left)){
			camera_angle -= camera_rotate_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Right)){
			camera_angle += camera_rotate_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::A)){
			camera_height += camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Z)){
			camera_height -= camera_speed * dt;
		}

		camera_pitch = xen::atan(camera_height / camera_radius);
		camera.position = {camera_radius * xen::cos(camera_angle), camera_height, camera_radius * xen::sin(camera_angle)};
		camera.look_dir = xen::normalized(-camera.position);
		camera.up_dir   = { 0, xen::cos(camera_pitch), xen::sin(camera_pitch) };
		printf("Pitch: %f, Up dir: (%f, %f, %f)\n", xen::asDegrees(camera_pitch), camera.up_dir.x, camera.up_dir.y, camera.up_dir.z);

		view_mat = getViewMatrix(camera, window_size);

		app.setActive(true);
		glClearColor(0.3,0.3,0.3,1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		xen::useShader(prog);

		model_mat  = Mat4r::Identity;
		model_mat *= xen::Rotation3dx(time * 41_deg);
		model_mat *= xen::Rotation3dy(time * 67_deg);
		model_mat *= xen::Rotation3dz(time * 83_deg);
		model_mat *= xen::Scale3d(1 + sin(time*15)*0.1, 1 + sin(time*15 + 0.25*xen::PI)*0.1, 1 + sin(time*15 + 0.5*xen::PI)*0.1);
		xen::setUniform(mvpMatLoc, model_mat * view_mat);
		renderCube();

		model_mat = Mat4r::Identity;
		model_mat *= xen::Scale3d(5, 0.05, 5);
		model_mat *= xen::Translation3d(0, -3, 0);
		xen::setUniform(mvpMatLoc, model_mat * view_mat);
		renderCube();

		app.display();
	}
	printf("Exiting main loop\n");

	return 0;
}

GLuint cube_vert_buffer;

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
    0.0f, 1.0f, 0.0f
};

void initCube(){
	glGenBuffers(1, &cube_vert_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, cube_vert_buffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(cube_buffer_data),
	             cube_buffer_data, GL_STATIC_DRAW);
}


void renderCube(){
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glBindBuffer(GL_ARRAY_BUFFER, cube_vert_buffer);
	glVertexAttribPointer(0,        // attrib layout
	                      3,        // components
	                      GL_FLOAT, // type
	                      GL_FALSE, // normalized
	                      0,        // stride
	                      (void*)0  // start offset
	                      );
	glVertexAttribPointer(1,        // attrib layout
	                      3,        // components
	                      GL_FLOAT, // type
	                      GL_TRUE,  // normalized
	                      0,        // stride
	                      (void*)(sizeof(float)*3*12*3)  // start offset
	                      );
	glDrawArrays(GL_TRIANGLES, 0, 12*3);
}

xen::ShaderProgram* loadShader(xen::ArenaLinear& arena){
	XenTempArena(scratch, 8196);

	FileData vertex_src = loadFileAndNullTerminate(scratch, "vertex.glsl");
	FileData pixel_src  = loadFileAndNullTerminate(scratch, "pixel.glsl");

	auto result = xen::createShaderProgram(arena, (char*)vertex_src.data, (char*)pixel_src.data);

	if(!xen::isOkay(result)){
		xen::resetArena(scratch);
		const char* errors = xen::getErrors(result, scratch);
		printf("Shader Errors:\n%s\n", errors);
	} else {
		printf("Shader compiled successfully\n");
	}

	return result;
}
