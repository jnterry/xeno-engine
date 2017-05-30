#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

int main(int argc, char** argv){

	sf::Window app(sf::VideoMode(640, 400, 32), "Window Title");

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

		app.display();
	}
	printf("Exiting main loop");



	return 0;
}
