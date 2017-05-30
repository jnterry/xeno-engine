#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/gl.h>

int main(int argc, char** argv){

	sf::Window app(sf::VideoMode(800, 600, 32), "Window Title");

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
	printf("Exiting main loop");

	return 0;
}
