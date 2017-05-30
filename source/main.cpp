#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/glew.h>
#include <GL/gl.h>

#include "xen/core/intrinsics.hpp"
#include "xen/core/memory.hpp"
#include "xen/graphics/Shader.hpp"
#include "xen/math/Vector.hpp"

int main(int argc, char** argv){

	xen::AllocatorCounter<xen::AllocatorMalloc> alloc;

	sf::Window app(sf::VideoMode(800, 600, 32), "Window Title");

	XenTempArena(arena, 4096);

	app.setActive(true);
	glewInit();

	xen::ShaderProgram* prog = xen::createShaderProgram(arena, "fdsfsd", "fdsfsd");
	if(!xen::isOkay(prog)){
		xen::MemoryTransaction transaction(arena);
		const char* errors = xen::getErrors(prog, arena);
		printf("Shader Errors:\n%s\n", errors);
	}

	printf("Entering main loop\n");
	while(app.isOpen()){
		sf::Event event;
		while(app.pollEvent(event)){
			switch(event.type){
			case sf::Event::Closed:
				app.close();
				break;
			default: break;
			}
		}

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glBegin(GL_TRIANGLES);
		glColor3d(1.0f, 1.0f, 1.0f);
		glVertex3f(0.0f, 0.0f, 0.0f);
		glVertex3f(0.0f, 1.0f, 0.0f);
		glVertex3f(1.0f, 0.0f, 0.0f);
		glEnd();

		app.display();
	}
	printf("Exiting main loop\n");

	return 0;
}
