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



	xen::Angle about_y = xen::atan(camera.look_dir.z / camera.look_dir.x);
	printf("About y: %f\n", xen::asDegrees(about_y));
	result *= xen::Rotation3dy(xen::clamp(about_y));

	// move world by negative of camera position
	result *= xen::Translation3d(-camera.position);


	result *= xen::createPerspectiveProjection(camera.fov_y, viewport_size.x, viewport_size.y, camera.z_near, camera.z_far);


	return result;
}
