////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains type for representing a camera in some 3d world and
/// declarations of functions for manipulating it
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_CAMERA3D_HPP
#define XEN_GRAPHICS_CAMERA3D_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix.hpp>

namespace xen {

	// other projection types:
	// http://www-f9.ijs.si/~matevz/docs/PovRay-3.50b/povdoc_182.html

	/// \brief Holds parameters for a perspective projection
	struct ProjectionPerspective{
		xen::Angle fov_y;      /// \brief Field of view in y direction
		real       z_near;     /// \brief Minumum distance to geometry from camera such that it is rendered
		real       z_far;      /// \brief Maximum distance to geometry from camera such that it is rendered
	};

	/// \brief Holds data representing a camera in 3d space
	struct Camera3d : public ProjectionPerspective{
		Vec3r      position; /// \brief Camera's position in 3d space
		Vec3r      look_dir; /// \brief Direction in which camera is looking

		/// \brief Direction considered to be "up" to the camera
		/// Note this is the direction in the world which is up, it need not be
		/// perpendicular to the look_dir
		Vec3r      up_dir;
	};

	/// \brief Represents a camera which orbits around some central target
	/// Camera operates as though fixed on a cylinder, oriented such that the
	/// cylinder's axis is defined by up_dir. The height is how far along
	/// the up_dir the camera is displaced. Radius and angle define the radius of the
	/// cylinder and the angle by which the camera is displaced around that radius.
	///
	/// Setting up_dir to (0,1,0) means camera can rotate around the target in the xz plane
	/// and additional rise up and below this plane as defined by its height
	struct Camera3dCylinder : public ProjectionPerspective{
		/// \brief Direction in the world considered to be "up" to the camera
		Vec3r      up_dir;

		/// \brief Center point that the camera is focusing on
		Vec3r      target;

		/// \brief Angle by which camera is rotated around up_dir
		xen::Angle angle;

		/// \brief Distance between camera and target in plane to which up_dir is the normal
		real       radius;

		/// \brief Distance camera is displaced along the up_dir axis
		real       height;
	};

	inline Camera3d generateCamera3d(const Camera3d& c){ return c; }

	/// \brief Generates a Camera3d corresponding to some Camera3dCylinder
	Camera3d generateCamera3d(const Camera3dCylinder&);

	/// \brief Gets matrix that transforms points in world space into camera relative space
	Mat4r getViewMatrix(const Camera3d& camera);

	/// \brief Helper function which generates a standard Camera3d from some other, then
	/// calls getViewMatrix
	template <typename T_CAM>
	Mat4r getViewMatrix(const T_CAM& camera){
		return getViewMatrix(generateCamera3d(camera));
	}

	/// \brief Gets matrix which projects points from camera relative space onto some viewport
	Mat4r getProjectionMatrix(const ProjectionPerspective& p, Vec2r viewport_size);

	template<typename T_CAM>
	Mat4r getViewProjectionMatrix(const T_CAM& cam, Vec2r viewport_size){
		Camera3d c = generateCamera3d(cam);
		return getViewMatrix(c) * getProjectionMatrix(c, viewport_size);
	}

	Vec3r getCameraPosition(const Camera3dCylinder& cam);
	inline Vec3r getCameraPosition(const Camera3d&      cam){ return cam.position; }
}

#endif
