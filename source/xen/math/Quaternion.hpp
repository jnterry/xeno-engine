////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Quaternion.hpp
/// \author Jamie Terry
/// \date 2017/06/01
/// \brief Contains type to represent Quaternions, as well as functions for
/// manipulating them. Additionally contains types and functions for representing
/// rotations as an axis about which to rotate, and the angle by which to rotate
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_QUATERNION_HPP
#define XEN_MATH_QUATERNION_HPP

#include <xen/core/intrinsics.hpp>
#include "Angle.hpp"
#include "Vector.hpp"
#include "Matrix.hpp"

namespace xen{
	struct Quaternion{
		union{
			real elements[4];
			real  x,y,z,w;
			Vec3r xyz;
			Vec4r xyzw;
		};
	};

	/// \brief Represents a rotation as an axis about which to rotate, and an angle
	struct AxisAngle{
		Vec3r axis;
		Angle angle;
	};
}

typedef xen::Quaternion Quat;

/// \brief Multiplies this Quaternion by the specifed vector and returns the result
/// as a new quaternion, does not modify either operand quaternion
xen::Quaternion operator*(xen::Quaternion q, const Vec3r& vec){
	return {  (q.w * vec.x) + (q.y * vec.z) - (q.z * vec.y)
		   ,  (q.w * vec.y) + (q.z * vec.x) - (q.x * vec.z)
		   ,  (q.w * vec.z) + (q.x * vec.y) - (q.y * vec.x)
		   , -(q.x * vec.x) - (q.y * vec.y) - (q.z * vec.z) };
}

xen::Quaternion operator*(xen::Quaternion lhs, xen::Quaternion rhs){
	return { (lhs.x * rhs.w) + (lhs.w * rhs.x) + (lhs.y * rhs.z) - (lhs.z * rhs.y)
		   , (lhs.y * rhs.w) + (lhs.w * rhs.y) + (lhs.z * rhs.x) - (lhs.x * rhs.z)
		   , (lhs.z * rhs.w) + (lhs.w * rhs.z) + (lhs.x * rhs.y) - (lhs.y * rhs.x)
		   , (lhs.w * rhs.w) - (lhs.x * rhs.x) - (lhs.y * rhs.y) - (lhs.z * rhs.z) };
}

namespace xen{
	/// \brief Constructs AxisAngle representation of rotation from a Quaterion
	inline AxisAngle toAxisAngle(Quaternion q){
		real mag = length(q.xyz);
		return { q.xyz / mag, xen::atan2(mag, q.w) * 2 };
	}

	/// \brief Constructs a quaterion from an axis about which to rotate, and an angle
	inline Quaternion fromAxisAngle(Vec3r axis, Angle a){
		a    *= 0.5;
		axis  = normalized(axis) * xen::sin(a);
		return { axis.x, axis.y, axis.z, xen::cos(a) };
	}
	inline Quaternion fromAxisAngle(AxisAngle aa){ return fromAxisAngle(aa.axis, aa.angle); }

	/// \brief Computes conjugate of some quaterion, IE: (-x, -y, -z, w)
	inline Quaternion conjugate(Quaternion q){
		return { -q.x, -q.y, -q.z, q.w };
	}

	/// \brief Computes inverse of some quaterion
	inline Quaternion inverse(Quaternion q){
		real mag = length(q.xyzw);
		return { q.x / mag, q.y / mag, q.z / mag, q.w / mag };
	}


	/// \brief Generates rotation matrix equivalent to some quaternion
	inline Mat4r Rotation3d(Quaternion q){
		return { 1.0_r - 2.0_r*(q.y*q.y - q.z*q.z)
			   ,         2.0_r*(q.x*q.y - q.z*q.w)
			   ,         2.0_r*(q.x*q.z + q.y*q.w)
			   , 0

			   ,         2.0_r*(q.x*q.y + q.z*q.w)
			   , 1.0_r - 2.0_r*(q.x*q.x - q.z*q.z)
			   ,         2.0_r*(q.y*q.z - q.x*q.w)
			   , 0

			   ,         2.0_r*(q.x*q.z - q.y*q.w)
			   ,         2.0_r*(q.y*q.z + q.x*q.w)
			   , 1.0_r - 2.0_r*(q.x*q.x - q.y*q.y)
			   , 0

			   , 0,0,0,1};
	}
	inline Mat4r Rotation3d(AxisAngle aa           ){ return Rotation3d(fromAxisAngle(aa         )); }
	inline Mat4r Rotation3d(Vec3r axis, Angle angle){ return Rotation3d(fromAxisAngle(axis, angle)); }

	inline Vec3r rotated(Vec3r v, Quaternion q){
		Quaternion r = q * v * conjugate(q);
		return { r.x, r.y, r.z };
	}
	inline Vec3r rotated(Vec3r v, AxisAngle aa       ){ return rotated(v, fromAxisAngle(aa     )); }
	inline Vec3r rotated(Vec3r v, Vec3r axis, Angle a){ return rotated(v, fromAxisAngle(axis, a)); }
}

#endif
