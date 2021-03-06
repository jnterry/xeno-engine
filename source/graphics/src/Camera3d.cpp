////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of camera related functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_CAMERA3D_CPP
#define XEN_GRAPHICS_CAMERA3D_CPP

#include <xen/math/vector.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/quaternion.hpp>
#include <cstdio>

#include <xen/graphics/Camera3d.hpp>

namespace xen {
	Mat4r getViewMatrix(const Camera3d& camera){
		//////////////////////////////////////////////////////////
		// Compute axes relative to the camera

		// camera looks down z axis
		Vec3r zaxis = -camera.look_dir;

		// up_dir and look_dir are two vectors within the plane containing camera's
		// local z and y axis, hence the camera's "right" vector is orthogonal to
		// this plane, so can be computed with a cross product
		Vec3r xaxis = xen::normalized(xen::cross(camera.look_dir, camera.up_dir));

		// Now we have the camera's x and z axis we can compute the yaxis as being
		// orthogonal to this plane
		Vec3r yaxis = xen::normalized(xen::cross(xaxis, zaxis));
		//////////////////////////////////////////////////////////

		Mat4r orientation = {
			xaxis.x, yaxis.x, zaxis.x, 0,
			xaxis.y, yaxis.y, zaxis.y, 0,
			xaxis.z, yaxis.z, zaxis.z, 0,
			0,       0,       0,       1
		};

		return xen::Translation3d(-camera.position) * orientation;
	}

	Mat4r getProjectionMatrix(const ProjectionPerspective& p, Vec2r viewport_size){
		return xen::createPerspectiveProjection(p.fov_y,
		                                        viewport_size.x, viewport_size.y,
		                                        p.z_near, p.z_far
		                                       );
	}

	Camera3d generateCamera3d(const Camera3dCylinder& cam){
		Camera3d result;

		*((ProjectionPerspective*)(&result)) = *((ProjectionPerspective*)(&cam));
		result.up_dir = xen::rotated(cam.up_dir, cam.axis, cam.angle);

		result.position = getCameraPosition(cam);

		result.look_dir = xen::normalized(cam.target - result.position);

		return result;
	}

	Camera3d generateCamera3d(const Camera3dSphere& cam){
		xen::Camera3d result;

		*((xen::ProjectionPerspective*)&result) = *((xen::ProjectionPerspective*)&cam);

		Vec3r cam_x, cam_y, cam_z;
		Vec3r radius = { 0, 0, cam.radius };

		cam_y = cam.y_axis;
		radius = xen::rotated(radius, cam.y_axis, cam.longitude);

		cam_x = xen::cross(cam_y, radius);
		radius = xen::rotated(radius, cam_x, cam.latitude);

		result.position = cam.target + radius;

		cam_y = xen::rotated(cam_y, cam_x, cam.latitude);
		cam_z = xen::normalized(cam.target - result.position);

		cam_z = xen::rotated(cam_z, cam_y, cam.yaw);
		cam_z = xen::rotated(cam_z, xen::cross(cam_z, cam_y), cam.pitch);

		result.up_dir   = cam_y;
		result.look_dir = cam_z;
		return result;
	}

	Vec3r getCameraPosition(const Camera3dCylinder& cam){
		// Start camera centered on the target
		Vec3r result = cam.target;

		// Displace by camera's height along up_dir axis
		result += xen::normalized(cam.axis) * cam.height;

		/////////////////////////////////////////////////////
		// Compute radius vector (IE: target to camera position)
		// Initially assume camera is looking down -ve z axis towards origin, and hence
		// is at some +ve z location
		Vec3r radius = {0, 0, cam.radius};
		// The rotate radius around axis by angle, add it on to displacement
		radius = xen::rotated(radius, cam.axis, cam.angle);
		// Add displacement onto the resulting position
		result += radius;

		return result;
	}

	bool isCameraValid(const Camera3d cam){
		return (cam.look_dir != Vec3r::Origin &&
		        cam.up_dir   != Vec3r::Origin &&
		        xen::normalized(cam.look_dir) != xen::normalized(cam.up_dir) &&
		        !xen::isnan(cam.look_dir) &&
		        !xen::isnan(cam.up_dir)   &&
		        !xen::isnan(cam.z_near)   &&
		        !xen::isnan(cam.z_far)
		       );
	}
}

#endif
