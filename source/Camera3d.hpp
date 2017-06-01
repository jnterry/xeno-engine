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
	Vec3r      position;   /// \brief Camera's position in 3d space
	Vec3r      look_dir;   /// \brief Direction in which camera is looking
	Vec3r      up_dir;     /// \brief Direction considered to be "up" to the camera
	xen::Angle fov_y;      /// \brief Field of view in y direction
	real       z_near;     /// \brief Minumum distance to geometry from camera such that it is rendered
	real       z_far;      /// \brief Maximum distance to geometry from camera such that it is rendered
};

/// \brief Gets matrix that transforms points in world space into camera relative space post projection
Mat4r getViewMatrix(Camera3d& camera, Vec2r viewport_size);

#endif
