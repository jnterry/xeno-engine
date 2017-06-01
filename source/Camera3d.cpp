////////////////////////////////////////////////////////////////////////////////
/// \file Camera3d.cpp
/// \author Jamie Terry
/// \date 2017/06/01
/// \brief Contains implementation of functions in Camera3d.hpp
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <cstdio>

#include "Camera3d.hpp"

Mat4r getViewMatrix(Camera3d& camera, Vec2r viewport_size){
	Mat4r result = Mat4r::Identity;

	// move world by negative of camera position
	result *= xen::Translation3d(-camera.position);

	// Line up z axis with look_dir
	xen::Angle about_y = xen::clockwiseAngleBetween(Vec3r::UnitZ, camera.look_dir);
	Mat4r rot_y = xen::Rotation3dy(-about_y);
	result *= rot_y;

	// Tilt camera up or down based on up direction
	xen::Angle about_x = xen::clockwiseAngleBetween(Vec3r::UnitY, -camera.up_dir) * xen::sign(camera.up_dir.z);
	printf("About_x: %f\n", asDegrees(about_x));
	result *= xen::Rotation3dx(about_x);

	// Do perspective projection
	result *= xen::createPerspectiveProjection(camera.fov_y, viewport_size.x, viewport_size.y, camera.z_near, camera.z_far);

	return result;
}
