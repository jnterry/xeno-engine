////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of utility functions used by example applications
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLE_UTILITES_CPP
#define XEN_EXAMPLE_UTILITES_CPP

#include "utilities.hpp"

#include <xen/graphics/Window.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/utilities.hpp>

void handleCameraInputCylinder(xen::Window* win, xen::Camera3dCylinder& camera, real dt, real max_radius){
	// compute speed such that can get from full zoom to no zoom in 3 seconds
	const real       camera_speed        = (max_radius / 3_r);
	const xen::Angle camera_rotate_speed = 120_deg;

	if(xen::isKeyPressed(xen::Key::ArrowUp, win)){
		camera.radius -= camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowDown, win)){
		camera.radius += camera_speed * dt;
	}
	camera.radius = xen::clamp(camera.radius, 0.005_r, max_radius);

	if(xen::isKeyPressed(xen::Key::ArrowLeft, win)){
		camera.angle -= camera_rotate_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowRight, win)){
		camera.angle += camera_rotate_speed * dt;
	}

	if(xen::isKeyPressed(xen::Key::A, win)){
		camera.height += camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::Z, win)){
		camera.height -= camera_speed * dt;
	}

	if(xen::isKeyPressed(xen::Key::Q, win)){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
	if(xen::isKeyPressed(xen::Key::E, win)){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

void handleCameraInputPlane(xen::Window* win, xen::Camera3d& camera, real dt) {
	const static constexpr real camera_speed = 1.0f;

	if(xen::isKeyPressed(xen::Key::ArrowUp, win)){
		camera.position.y += camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowDown, win)){
	  camera.position.y -= camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowLeft, win)){
		camera.position.x += camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowRight, win)){
		camera.position.x -= camera_speed * dt;
	}

	if(xen::isKeyPressed(xen::Key::A, win)){
		camera.position.z -= camera_speed * dt;
		camera.position.z = xen::max(0.001_r, camera.position.z);
	}
	if(xen::isKeyPressed(xen::Key::Z, win)){
		camera.position.z += camera_speed * dt;
	}

	if(xen::isKeyPressed(xen::Key::Q, win)){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
	if(xen::isKeyPressed(xen::Key::E, win)){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

#endif
