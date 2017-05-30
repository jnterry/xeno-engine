#include <stdio.h>

#include <SFML/System/Clock.hpp>

int main(int argc, char** argv){
	printf("Hello World\n");
	sf::Clock clock;
	while(clock.getElapsedTime().asSeconds() < 5){
		printf("Elapsed: %f\n", clock.getElapsedTime().asSeconds());
	}
	return 0;
}
