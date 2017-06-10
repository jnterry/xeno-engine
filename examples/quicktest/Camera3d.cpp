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

	// :TODO: assert angle is 90
	//printf("Angle between camera up/look dir: %f\n", xen::minAngleBetween(camera.look_dir, camera.up_dir));

	Mat4r result = Mat4r::Identity;

	// move world by negative of camera position
	result *= xen::Translation3d(-camera.position);


	// :TODO: look_dir and up_dir work independently, but not together
	// IE: look_dir works assuming up_dir   is UnitY
	//     up_dir   works assuming look_dir is UnitZ
	// but produce weird results otherwise

	// Line up z axis with look_dir
	Quat rot = xen::getRotation(camera.look_dir, -Vec3r::UnitZ);
	//rot = xen::getRotation(camera.up_dir, Vec3r::UnitY);

	//printf("Z rot: (%f,%f,%f,%f)\n", rot.x, rot.y, rot.z, rot.w);
	result *= xen::Rotation3d(rot);

	// Tilt camera up or down based on up direction
	//xen::Angle about_x = xen::clockwiseAngleBetween(Vec3r::UnitY, -camera.up_dir) * xen::sign(camera.up_dir.z);
	//result *= xen::Rotation3dx(about_x);

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
