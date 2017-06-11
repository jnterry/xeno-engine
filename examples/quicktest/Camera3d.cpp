////////////////////////////////////////////////////////////////////////////////
/// \file Camera3d.cpp
/// \author Jamie Terry
/// \date 2017/06/01
/// \brief Contains implementation of functions in Camera3d.hpp
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Quaternion.hpp>
#include <cstdio>

#include "Camera3d.hpp"

Mat4r getViewMatrix(const Camera3d& camera){
	// move world by negative of camera position
	Mat4r result = xen::Translation3d(-camera.position);

	// Line up z axis with look_dir, and y with up_dir
	Quat rot = xen::getRotation(camera.look_dir, camera.up_dir,   -Vec3r::UnitZ, Vec3r::UnitY);
	result *= xen::Rotation3d(rot);

	return result;
}

Mat4r getProjectionMatrix(const Camera3d& camera, Vec2r viewport_size){
	return xen::createPerspectiveProjection(camera.fov_y, viewport_size.x, viewport_size.y, camera.z_near, camera.z_far);
}

Camera3d generateCamera3d(const Camera3dOrbit& cam){
	Camera3d result;

	result.fov_y  = cam.fov_y;
	result.z_near = cam.z_near;
	result.z_far  = cam.z_far;
	result.up_dir = cam.up_dir;

	// Start camera centered on the target
	result.position  = cam.target;

	// Displace by camera's height along up_dir axis
	result.position += xen::normalized(cam.up_dir) * cam.height;

	// Displace by radius rotated around the up_dir
	// find radius vector by rotating (1,0,0) to have +Y line up with up_dir
	Vec3r radius = xen::rotated(Vec3r(cam.radius, 0, 0), getRotation(Vec3r::UnitY, cam.up_dir));
	// The rotate radius around up_dir by angle, add it on to displacement
	result.position += xen::rotated(radius, cam.up_dir, cam.angle);

	result.look_dir = xen::normalized(cam.target - result.position);

	return result;
}
