////////////////////////////////////////////////////////////////////////////////
/// \file Camera3d.hpp
/// \author Jamie Terry
/// \date 2017/05/31
/// \brief Contains type for representing camera and functions for manipulating it
////////////////////////////////////////////////////////////////////////////////

#ifndef CAMERA3D_HPP
#define CAMERA3D_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>

/// \brief Holds data representing a camera in 3d space
struct Camera3d{
    xen::Angle fov_y;      /// \brief Field of view in y direction
	real       z_near;     /// \brief Minumum distance to geometry from camera such that it is rendered
	real       z_far;      /// \brief Maximum distance to geometry from camera such that it is rendered

	Vec3r      position;   /// \brief Camera's position in 3d space
	Vec3r      look_dir;   /// \brief Direction in which camera is looking

	/// \brief Direction considered to be "up" to the camera
	/// Note this is the direction in the world which is up, it need not be
	/// perpendicular to the look_dir
	Vec3r      up_dir;
};

/// \brief Represents a camera which orbits around some central target
/// Camera can rotate around the target in the xz plane, and additional rise up and below
/// this plane (this is the camera's height)
struct Camera3dOrbit{
	xen::Angle fov_y;      /// \brief Field of view in y direction
	real       z_near;     /// \brief Minumum distance to geometry from camera such that it is rendered
	real       z_far;      /// \brief Maximum distance to geometry from camera such that it is rendered

	Vec3r      up_dir; /// \brief Direction considered to be "up" to the camera
	Vec3r      target; /// \brief Center point that the camera is focusing on
	xen::Angle angle;  /// \brief Angle the camera with z axis in xy plane
	real       radius; /// \brief Distance between camera and target in xz plane
	real       height; /// \brief Height above/below the xz plane
};

/// \brief Generates a Camera3d corresponding to some Camera3dOrbit
Camera3d generateCamera3d(const Camera3dOrbit&);

/// \brief Helper function which generates a standard Camera3d from some other, then
/// calls getViewMatrix
template <typename T_CAM>
Mat4r getViewMatrix(const T_CAM& camera, Vec2r viewport_size){
	return getViewMatrix(generateCamera3d(camera), viewport_size);
}

/// \brief Gets matrix that transforms points in world space into camera relative space post projection
Mat4r getViewMatrix(const Camera3d& camera, Vec2r viewport_size);

#endif
