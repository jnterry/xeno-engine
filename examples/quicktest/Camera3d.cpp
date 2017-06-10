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

Mat4r getViewMatrix(const Camera3d& camera, Vec2r viewport_size){
	Mat4r result = Mat4r::Identity;

	// move world by negative of camera position
	result *= xen::Translation3d(-camera.position);

	// Line up z axis with look_dir, and y with up_dir
	Quat rot = xen::getRotation(camera.look_dir, camera.up_dir,   -Vec3r::UnitZ, Vec3r::UnitY);
	result *= xen::Rotation3d(rot);

	//Vec3r up_dir_t = xen::rotated(camera.up_dir, rot);

	// Line up y axis with up_dir
	//result *= xen::Rotation3d(xen::getRotation(xen::rotated(camera.up_dir, rot), Vec3r::UnitY));

	//printf("Up dir: (%f, %f, %f), (%f, %f, %f)\n",
	//       camera.up_dir.x, camera.up_dir.y, camera.up_dir.z,
	                                           //       up_dir_t.x, up_dir_t.y, up_dir_t.z);

	// Do perspective projection
	result *= xen::createPerspectiveProjection(camera.fov_y, viewport_size.x, viewport_size.y, camera.z_near, camera.z_far);

	return result;
}

Camera3d generateCamera3d(const Camera3dOrbit& cam){
	Camera3d result;

	result.fov_y  = cam.fov_y;
	result.z_near = cam.z_near;
	result.z_far  = cam.z_far;

	result.position  = cam.target;
	result.position += Vec3r{cam.radius * xen::cos(cam.angle), cam.height, cam.radius * xen::sin(cam.angle)};

	result.look_dir = xen::normalized(cam.target - result.position);
	auto pitch = xen::atan(cam.height / cam.radius);
	result.up_dir   = Vec3r{ 0, xen::cos(pitch), xen::sin(pitch) };

	return result;
}
