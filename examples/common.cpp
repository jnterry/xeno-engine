////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains helper functions common to all example applications
///
/// \ingroup examples
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLES_COMMON_CPP
#define XEN_EXAMPLES_COMMON_CPP

#include <xen/graphics/Window.hpp>

const real       camera_speed        = 250;
const xen::Angle camera_rotate_speed = 120_deg;

void handleCameraInput(xen::Camera3dCylinder& camera, real dt){

	if(xen::isKeyPressed(xen::Key::ArrowUp)){
		camera.radius -= camera_speed * dt;
	}
	if(xen::isKeyPressed(xen::Key::ArrowDown)){
		camera.radius += camera_speed * dt;
	}
	camera.radius = xen::clamp(camera.radius, 0.01_r, 750_r);

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


#endif
