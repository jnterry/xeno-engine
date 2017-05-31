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

static const char* vertex_shader_src =
                                   "#version 330\n"
                                   "layout(location = 0) in vec3 pos;\n"
                                   "layout(location = 1) in vec3 vert_color;\n"
                                   "varying vec3 color;"
                                   "uniform mat4 mvpMatrix;\n"
                                   "void main(){\n"
                                   "  color = vert_color;\n"
                                   "  gl_Position = mvpMatrix * vec4(pos,1);\n"
                                   "}";

static const char* fragment_shader_src =
                                   "#version 330\n"
                                   "out vec4 out_color;\n"
                                   "varying vec3 color;\n"
                                   "void main(){\n"
                                   "  out_color = vec4(color,1);\n"
                                   "}";


void initCube();
void renderCube();

int main(int argc, char** argv){

	xen::AllocatorCounter<xen::AllocatorMalloc> alloc;


	sf::ContextSettings context_settings;
	context_settings.depthBits = 24;
	context_settings.stencilBits = 8;
	context_settings.antialiasingLevel = 4;
	context_settings.majorVersion = 3;
	context_settings.minorVersion = 0;
	sf::Window app(sf::VideoMode(800, 600, 32), "Window Title", sf::Style::Default, context_settings);

	XenTempArena(arena, 4096);

	app.setActive(true);
	glewInit();
	initCube();

	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);

	auto view_mat = xen::createPerspectiveProjection(xen::Degrees(80), 800, 600, 0.001, 10000);

	xen::ShaderProgram* prog = xen::createShaderProgram(arena, vertex_shader_src, fragment_shader_src);
	if(!xen::isOkay(prog)){
		xen::MemoryTransaction transaction(arena);
		const char* errors = xen::getErrors(prog, arena);
		printf("Shader Errors:\n%s\n", errors);
	} else {
		printf("Shader compiled successfully\n");
	}
	int mvpMatLoc = xen::getUniformLocation(prog, "mvpMatrix");
	Mat4r model_mat;

	sf::Clock timer;
	printf("Entering main loop\n");
	while(app.isOpen()){
		sf::Event event;
		while(app.pollEvent(event)){
			switch(event.type){
			case sf::Event::Closed:
				app.close();
				break;
			case sf::Event::Resized:
				glViewport(0,0,event.size.width, event.size.height);
				view_mat = xen::createPerspectiveProjection(xen::Degrees(80), 800, 600, 0.001, 10000);
				break;
			default: break;
			}
		}

		float time = timer.getElapsedTime().asSeconds();
		app.setActive(true);
		xen::useShader(prog);
		model_mat  = Mat4r::Identity;
		model_mat *= xen::Rotation3dx<real>(xen::Degrees(time * 20.0f));
		model_mat *= xen::Rotation3dz<real>(xen::Degrees(time * 30.0f));
		model_mat *= xen::Scale3d<real>(1 + sin(time*10)*0.1, 1 + sin(time*10 + 0.25*xen::PI)*0.1, 1 + sin(time*10 + 0.5*xen::PI)*0.1);
		model_mat *= xen::Translation3d<real>(0.0, 0.0, -5);
		xen::setUniform(mvpMatLoc, model_mat * view_mat);
		glClearColor(1,1,0,1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

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
    1.0f, 1.0f, 1.0f, // triangle 1 : begin
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f, // triangle 1 : end
    1.0f, 1.0f, 1.0f, // triangle 2 : begin
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f, // triangle 2 : end
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f
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
	                      GL_FALSE, // normalized
	                      0,        // stride
	                      (void*)0  // start offset
	                      );
	glDrawArrays(GL_TRIANGLES, 0, 12*3);
}
