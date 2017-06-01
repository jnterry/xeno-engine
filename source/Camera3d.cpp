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

	Vec3r cam_z = camera.look_dir;
	Vec3r cam_y = camera.up_dir;
	//Vec3r cam_x = xen::cross(cam_y, cam_z);

	Vec3r neg_pos = camera.position;

	Mat4r result = Mat4r::Identity;

	// move world by negative of camera position
	result *= xen::Translation3d(-camera.position);

	xen::Angle about_y = xen::angleBetween(Vec3r::UnitZ, cam_z);
	printf("Pos: (%f, %f), about y: %f\n", camera.position.x, camera.position.z, xen::asDegrees(about_y));
	result *= xen::Rotation3dy(about_y);


	result *= xen::createPerspectiveProjection(camera.fov_y, viewport_size.x, viewport_size.y, camera.z_near, camera.z_far);

	return result;
}
