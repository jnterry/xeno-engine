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
#include <xen/math/vector.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/geometry_types.hpp>

namespace xen {

	// other projection types:
	// http://www-f9.ijs.si/~matevz/docs/PovRay-3.50b/povdoc_182.html

	/////////////////////////////////////////////////////////////////////
	/// \brief Holds parameters for a perspective projection
	/////////////////////////////////////////////////////////////////////
	struct ProjectionPerspective{
		/// \brief Field of view in y direction
		xen::Angle fov_y;

		/// \brief Minimum distance to geometry from camera such that it is rendered
		real       z_near;

		/// \brief Maximum distance to geometry from camera such that it is rendered
		real       z_far;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Holds data representing a camera in 3d space
	/////////////////////////////////////////////////////////////////////
	struct Camera3d : public ProjectionPerspective{
		/// \brief Camera's position in 3d space
		Vec3r      position;

		/// \brief Direction in which camera is looking
		Vec3r      look_dir;

		/// \brief Direction considered to be "up" to the camera
		/// Note this is the direction in the world which is up, it need not be
		/// perpendicular to the look_dir
		Vec3r      up_dir;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a camera which orbits around some central target.
	///
	/// The camera operates as though fixed upon a cylinder, oriented such that
	/// the cylinder's axis is defined by `axis`. The `height` is how far along
	/// the axis the camera is displaced. Radius and angle define the radius of
	/// the cylinder and the angle around the axis  by which the camera is
	/// displaced by that radius.
	///
	/// Setting axis to (0,1,0) means camera can rotate around the target in the
	/// xz plane and additional rise up and below this plane as defined by its
	/// height
	///
	/// Note that the up_direction for the camera is specified separately to the
	/// axis such that the camera may yaw without changing the cylinder upon which
	/// it moves
	/////////////////////////////////////////////////////////////////////
	struct Camera3dCylinder : public ProjectionPerspective{
		/// \brief Direction in the world considered to be "up" to the camera
		Vec3r      up_dir;

		/// \brief Direction along which the cylinder's axis runs.
		/// Camera will move by "height" units along this direction
		Vec3r      axis;

		/// \brief Center point that the camera is focusing on
		Vec3r      target;

		/// \brief Angle by which camera is rotated around up_dir
		xen::Angle angle;

		/// \brief Distance between camera and target in plane to which up_dir is the normal
		real       radius;

		/// \brief Distance camera is displaced along the up_dir axis
		real       height;
	};

	/// \brief Represents a camera which moves on a sphere around some target point
	struct Camera3dSphere : public xen::ProjectionPerspective{
		/// \brief The object being rotated around
		Vec3r      target;

		/// \brief The y_axis to use for the camera, determines up_dir (although
		/// up_dir will only equal y_axis when the camera is on the equator of
		/// the sphere)
		Vec3r      y_axis;

		/// \brief Radius of the virtual sphere the camera is moving on
		real       radius;

		/// \brief Angle to rotate around the equator of the sphere
		xen::Angle longitude;

		/// \brief Angle to rotate towards the poles of the sphere
		xen::Angle latitude;

		/// \brief Angle to perturb the look_dir by in the x direction
		/// If set to 0 degrees camera will look directly at target point,
		/// if set to 90 degrees camera will be looking along the tangent
		/// to the sphere at its current position
		xen::Angle yaw;

		/// \brief Angle to perturb the look_dir by in the z direction
		/// If set to 0 degrees camera will look directly at target point,
		/// if set to 90 degrees camera will be looking along the tangent
		/// to the sphere at its current position
		xen::Angle pitch;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a standard Camera3d from some other type
	/// This allows the user of Xeno Engine to represent the camera however is
	/// most convenient to their application, and then convert to a Camera3d for
	/// rendering at the last possible moment
	///
	/// \public \memberof Camera3d
	/// \public \memberof Camera3dCylinder
	/////////////////////////////////////////////////////////////////////
	inline Camera3d generateCamera3d(const Camera3d& c){ return c; }

	/////////////////////////////////////////////////////////////////////
	/// \overload generateCamera3d
	/////////////////////////////////////////////////////////////////////
	Camera3d generateCamera3d(const Camera3dCylinder&);
	Camera3d generateCamera3d(const Camera3dSphere&);

	/////////////////////////////////////////////////////////////////////
	/// \brief Gets matrix that transforms points in world space into
	/// camera relative space
	///
	/// \public \memberof Camera3d
	/////////////////////////////////////////////////////////////////////
	Mat4r getViewMatrix(const Camera3d& camera);

	/////////////////////////////////////////////////////////////////////
	/// \brief Helper function which generates a standard Camera3d from some other
	/// and then calls getViewMatrix
	///
	/// \see getProjectionMatrix
	/// \see getViewMatrix
	///
	/// \public \memberof Camera3dCylinder
	/////////////////////////////////////////////////////////////////////
	template <typename T_CAM>
	Mat4r getViewMatrix(const T_CAM& camera){
		return getViewMatrix(generateCamera3d(camera));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Gets matrix which projects points from camera relative space
	/// onto some viewport
	///
	/// \public \memberof Camera3d
	///
	/// \todo :TODO: is this true? in software renderer we do additional steps
	/// to map to view port after the projection matrix...
	///
	/// \see getViewProjectionMatrix
	/// \see getViewMatrix
	/////////////////////////////////////////////////////////////////////
	Mat4r getProjectionMatrix(const ProjectionPerspective& p, Vec2r viewport_size);
	inline Mat4r getProjectionMatrix(const ProjectionPerspective& p, Aabb2u viewport){
		return getProjectionMatrix(p, (Vec2r)(viewport.max - viewport.min));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Helper function which generates a standard Camera3d from some
	/// other and then calls getViewProjectionMatrix
	///
	/// \public \memberof Camera3d
	///
	/// \see getViewMatrix
	/// \see getProjectionMatrix
	/////////////////////////////////////////////////////////////////////
	template<typename T_CAM>
	Mat4r getViewProjectionMatrix(const T_CAM& cam, Vec2r viewport_size){
		Camera3d c = generateCamera3d(cam);
		return getViewMatrix(c) * getProjectionMatrix(c, viewport_size);
	}

	template<typename T_CAM>
	Mat4r getViewProjectionMatrix(const T_CAM& cam, Aabb2u viewport){
		return getViewProjectionMatrix(cam, (Vec2r)(viewport.max - viewport.min));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines the 3d position of a camera in world space
	/////////////////////////////////////////////////////////////////////
	Vec3r        getCameraPosition(const Camera3dCylinder& cam);

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines the 3d position of a camera in world space
	/////////////////////////////////////////////////////////////////////
	inline Vec3r getCameraPosition(const Camera3d&         cam){
		return cam.position;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines if a camera is valid and may be used for rendering
	/// IE: No NaN values, look_dir and up_dir are not in the same direction,
	/// etc
	/////////////////////////////////////////////////////////////////////
	bool isCameraValid(const Camera3d cam);
}

#endif
