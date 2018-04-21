////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains helper functions common to all example applications
///
/// \ingroup examples
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLES_COMMON_CPP
#define XEN_EXAMPLES_COMMON_CPP

#include <xen/sren/SoftwareDevice.hpp>
#include <xen/gl/GlDevice.hpp>

#include "common.hpp"

const char* ExampleApplication::BACKEND_NAMES[Backend::COUNT] = {
	"Software Rasterizer",
	"Software Raytracer",
	"OpenGL",
	"Raytracer Camera Debug",
	"Software Atom Tracer",
	"Software Atom Tracer Debug",
};

ExampleApplication createApplication(const char* window_title,
                                     ExampleApplication::Backend default_backend,
                                     xen::Array<xen::sren::PostProcessor*> post_processors){
	xen::Allocator* alloc = new xen::AllocatorCounter<xen::AllocatorMalloc>();
	ExampleApplication app = {
		alloc,
		xen::createArenaLinear(*alloc, xen::megabytes(32)),
		nullptr,
		nullptr,
	};

	Vec2u window_size = {800, 600};

	do {
		bool option_picked  = false;
		int backend         = 0;

		// change to 0 to disable picking a backend - default_backend will be used
		#if 1
		printf("Select graphics backend\n");
		printf("-----------------------\n");
		for(u32 i = 0; i < ExampleApplication::Backend::COUNT; ++i){
			printf("%2i. %s\n", i+1, ExampleApplication::BACKEND_NAMES[i]);
		}
		printf("\n");
		printf("Enter Choice");
		if(default_backend >= 0 &&
		   default_backend < ExampleApplication::Backend::COUNT
		  ){
			printf(" (press enter for default: %i)", default_backend+1);
		}
		printf(": ");


		char c              = '\0';
		bool invalid_option = false;
		while(true){
			c = getchar();
			if(c == '\n'){ break; }
			if(c < '0' || c > '9'){
				invalid_option = true;
				printf("\nYou must enter a valid integer choice!\n\n");
				break;
			}
			option_picked = true;
			backend *= 10;
			backend += (c - '0');
		}

		if(invalid_option){
			// Then re-print the menu, prompt user again
			continue;
		}
		#endif

		if(option_picked){
			--backend; // Convert from gui [1, n+1] range to [0, n] range
		} else {
			if(default_backend >= 0 &&
			   default_backend < ExampleApplication::Backend::COUNT){
				backend = default_backend;
				printf("Using default option: %s\n",
				       ExampleApplication::BACKEND_NAMES[backend]);
			} else {
				printf("Please enter valid integer choice\n");
				continue;
			}
		}

		switch(backend){
		case ExampleApplication::Backend::RASTERIZER:
			app.device = xen::createRasterizerDevice(app.arena, post_processors);
			break;
		case ExampleApplication::Backend::RAYTRACER:
			window_size = {400, 400};
			app.device = xen::createRaytracerDevice(app.arena, post_processors);
			break;
		case ExampleApplication::Backend::OPENGL:
			app.device = xen::createGlDevice(app.arena);
			break;
		case ExampleApplication::Backend::RAYTRACER_CAMERA_DEBUG:
			window_size.x = 800;
			window_size.y = 800;
			app.device = xen::createRaytracerDebugDevice(app.arena,
			                                             10, &Vec3r::Origin);
			break;
		case ExampleApplication::Backend::ATOMTRACER:
			app.device = xen::createAtomTracerDevice(app.arena, post_processors);
			break;
		case ExampleApplication::Backend::ATOMTRACER_DEBUG:
			app.device = xen::createAtomTracerDebugDevice(app.arena, post_processors);
			break;
		default:
			printf("\nInvalid choice, please select an option from the list\n\n");
			break;
		}
	} while(app.device == nullptr);
	app.window = app.device->createWindow(window_size, window_title);

	return app;
}

void destroyApplication(ExampleApplication& app){
	xen::destroyArenaLinear(*app.allocator, app.arena);
	delete app.allocator;
}

void handleCameraInputCylinder(xen::Camera3dCylinder& camera, real dt, real max_radius){
	// compute speed such that can get from full zoom to no zoom in 3 seconds
	const real       camera_speed        = (max_radius / 3_r);
	const xen::Angle camera_rotate_speed = 120_deg;

	if(xen::isKeyPressed(xen::Key::ArrowUp)){
		camera.radius -= camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowDown)){
		camera.radius += camera_speed * dt;
	}
	camera.radius = xen::clamp(camera.radius, 0.005_r, max_radius);

  if(xen::isKeyPressed(xen::Key::ArrowLeft)){
		camera.angle -= camera_rotate_speed * dt;
	}
  if(xen::isKeyPressed(xen::Key::ArrowRight)){
		camera.angle += camera_rotate_speed * dt;
	}

  if(xen::isKeyPressed(xen::Key::A)){
		camera.height += camera_speed * dt;
	}
  if(xen::isKeyPressed(xen::Key::Z)){
		camera.height -= camera_speed * dt;
	}

  if(xen::isKeyPressed(xen::Key::Q)){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
  if(xen::isKeyPressed(xen::Key::E)){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

void handleCameraInputPlane(xen::Camera3d& camera, real dt) {
	const static constexpr real camera_speed = 1.0f;

	if(xen::isKeyPressed(xen::Key::ArrowUp)){
		camera.position.y += camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowDown)){
	  camera.position.y -= camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowLeft)){
		camera.position.x += camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowRight)){
		camera.position.x -= camera_speed * dt;
	}

	if(xen::isKeyPressed(xen::Key::A)){
		camera.position.z -= camera_speed * dt;
		camera.position.z = xen::max(0.001_r, camera.position.z);
	}
	if(xen::isKeyPressed(xen::Key::Z)){
		camera.position.z += camera_speed * dt;
	}

	if(xen::isKeyPressed(xen::Key::Q)){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
  if(xen::isKeyPressed(xen::Key::E)){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

FpsCounter::FpsCounter() : counter (0) {
	timer.restart();
}

void FpsCounter::update(){
	++counter;

	if(xen::asSeconds<real>(timer.getElapsedTime()) > 0.5_r){
		real fps = counter / xen::asSeconds<real>(timer.getElapsedTime());
		printf("FPS: %f\n", fps);
		counter = 0;
	  timer.restart();
	}
}

#endif
