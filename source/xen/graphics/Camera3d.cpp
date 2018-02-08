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
		// move world by negative of camera position
		Mat4r result = xen::Translation3d(-camera.position);

		// Line up z axis with look_dir, and y with up_dir
		Quat rot = xen::getRotation(camera.look_dir, camera.up_dir,   -Vec3r::UnitZ, Vec3r::UnitY);
		result *= xen::Rotation3d(rot);

		return result;
	}

	Mat4r getProjectionMatrix(const ProjectionPerspective& p, Vec2r viewport_size){
		return
			//xen::Translation3d(-viewport_size.x / 2.0f, -viewport_size.y, 0) *
			xen::createPerspectiveProjection(p.fov_y, viewport_size.x, viewport_size.y, p.z_near, p.z_far)

			;


	}

	Camera3d generateCamera3d(const Camera3dOrbit& cam){
		Camera3d result;

		*((ProjectionPerspective*)(&result)) = *((ProjectionPerspective*)(&cam));
		result.up_dir = cam.up_dir;

		result.position = getCameraPosition(cam);

		result.look_dir = xen::normalized(cam.target - result.position);

		return result;
	}

	Vec3r getCameraPosition(const Camera3dOrbit& cam){
		// Start camera centered on the target
		Vec3r result = cam.target;

		// Displace by camera's height along up_dir axis
		result += xen::normalized(cam.up_dir) * cam.height;

		// Displace by radius rotated around the up_dir
		// find radius vector by rotating (1,0,0) to have +Y line up with up_dir
		Vec3r radius = xen::rotated(Vec3r(cam.radius, 0, 0), getRotation(Vec3r::UnitY, cam.up_dir));
		// The rotate radius around up_dir by angle, add it on to displacement
		result += xen::rotated(radius, cam.up_dir, cam.angle);

		return result;
	}
}

#endif
