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
#include <xen/math/Angle.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/matrix_types.hpp>

// gcc doesn't like the anonomous structures inside unions, disable the warning temporarily...
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

namespace xen{
	struct Quaternion;

	/// \brief Represents a rotation as an axis about which to rotate, and an angle
	struct AxisAngle{
		AxisAngle() {}
		AxisAngle(Vec3r naxis, Angle nangle) : axis(naxis), angle(nangle) {}
		AxisAngle(Quaternion q);
		Vec3r axis;
		Angle angle;
	};

	/// \brief Represents a quaternion, coefficents stored in i,j,k, real order
	struct Quaternion{
		Quaternion() {}
		Quaternion(real nx, real ny, real nz, real nw) : x(nx), y(ny), z(nz), w(nw) {}
		Quaternion(Vec3r axis, Angle a){
			a *= 0.5;
			this->xyz = normalized(axis) * xen::sin(a);
			this->w   = xen::cos(a);
		}
		Quaternion(AxisAngle aa) : Quaternion(aa.axis, aa.angle) {}
		union{
			real elements[4];
			struct { real x,y,z,w; };
			struct { real i,j,k,r; }; // i,j,k and real components
			Vec3r xyz;
			Vec4r xyzw;
		};

		/// \brief Quaternion which represents 0 rotation
		static const Quaternion Identity;
	};
}

#pragma GCC diagnostic pop // re-enable -Wpedantic

typedef xen::Quaternion Quat;

inline xen::Quaternion operator*(xen::Quaternion lhs, xen::Quaternion rhs){
	return { (lhs.x * rhs.w) + (lhs.w * rhs.x) + (lhs.y * rhs.z) - (lhs.z * rhs.y)
		   , (lhs.y * rhs.w) + (lhs.w * rhs.y) + (lhs.z * rhs.x) - (lhs.x * rhs.z)
		   , (lhs.z * rhs.w) + (lhs.w * rhs.z) + (lhs.x * rhs.y) - (lhs.y * rhs.x)
		   , (lhs.w * rhs.w) - (lhs.x * rhs.x) - (lhs.y * rhs.y) - (lhs.z * rhs.z) };
}

inline xen::Quaternion& operator*=(xen::Quaternion& lhs, const xen::Quaternion& rhs){
	lhs = (lhs * rhs);
	return lhs;
}

/// \brief Multiplies this Quaternion by the specifed vector and returns the result
/// as a new quaternion, does not modify either operand quaternion
inline xen::Quaternion operator*(xen::Quaternion q, const Vec3r& vec){
	return q * xen::Quaternion{vec.x, vec.y, vec.z, 1};
}

inline xen::Quaternion operator/(xen::Quaternion q, real s){
	return { q.x / s, q.y / s, q.z / s, q.w / s };
}

inline bool operator==(xen::Quaternion l, xen::Quaternion r){ return l.xyzw == r.xyzw; }
inline bool operator!=(xen::Quaternion l, xen::Quaternion r){ return l.xyzw != r.xyzw; }
inline bool operator==(xen::AxisAngle  l, xen::AxisAngle  r){
	return l.axis == r.axis && l.angle == r.angle;
}
inline bool operator!=(xen::AxisAngle  l, xen::AxisAngle  r){
	return l.axis != r.axis || l.angle != r.angle;
}

namespace xen{
	/// \brief Computes conjugate of some quaterion, IE: (-x, -y, -z, w)
	inline Quaternion conjugate(Quaternion q){
		return { -q.x, -q.y, -q.z, q.w };
	}

	inline real mag(const Quaternion& q){ return mag(q.xyzw); }

	inline Quaternion normalized(const Quaternion& q){
		return q / mag(q);
	}

	/// \brief Computes inverse of some quaterion
	inline Quaternion inverse(Quaternion q){
		real mag = length(q.xyzw);
		return { q.x / mag, q.y / mag, q.z / mag, q.w / mag };
	}




	/// \brief Generates rotation matrix equivalent to some quaternion
	/// \warn  Undefined if q is not normalized
	inline Mat4r Rotation3d(Quaternion q){
		q = conjugate(q);
		return { 1.0_r - 2.0_r*(q.y*q.y + q.z*q.z)
			   ,         2.0_r*(q.x*q.y - q.z*q.w)
			   ,         2.0_r*(q.x*q.z + q.y*q.w)
			   , 0

			   ,         2.0_r*(q.x*q.y + q.z*q.w)
			   , 1.0_r - 2.0_r*(q.x*q.x + q.z*q.z)
			   ,         2.0_r*(q.y*q.z - q.x*q.w)
			   , 0

			   ,         2.0_r*(q.x*q.z - q.y*q.w)
			   ,         2.0_r*(q.y*q.z + q.x*q.w)
			   , 1.0_r - 2.0_r*(q.x*q.x + q.y*q.y)
			   , 0

			   , 0,0,0,1};
	}
	inline Mat4r Rotation3d(AxisAngle aa           ){ return Rotation3d(Quat(aa         )); }
	inline Mat4r Rotation3d(Vec3r axis, Angle angle){ return Rotation3d(Quat(axis, angle)); }

	inline Vec3r rotated(Vec3r v, Quaternion q){
		Quaternion r = (q * v) * conjugate(q);
		return { r.x, r.y, r.z };
	}
	inline Vec3r rotated(Vec3r v, AxisAngle aa       ){ return rotated(v, Quat(aa     )); }
	inline Vec3r rotated(Vec3r v, Vec3r axis, Angle a){ return rotated(v, Quat(axis, a)); }

	inline Quaternion getRotation(const Vec3r& start, const Vec3r& dest){
		real k_cos_theta = dot(start, dest);
		real k = sqrt(lengthSq(start) * lengthSq(dest));

		if (k_cos_theta / k == -1) {
			// Then vectors point in opposite directions, return quaternion
			// representing rotation of 180 deg around arbitary axis
			return { 0,0,0, -1};
		} else {
			Vec3r axis = cross(start, dest);
			return xen::normalized({ axis.x, axis.y, axis.z, k_cos_theta + k });
		}
	}

	/// \brief Gets rotation which maps vector pair (u0, v0) onto (u2, v2)
	/// This assumes the angle (u0, v0) is the same as (u2, v2)
	inline Quaternion getRotation(const Vec3r& u0, const Vec3r& v0, const Vec3r u2, const Vec3r v2){
		// taken from https://stackoverflow.com/questions/19445934/quaternion-from-two-vector-pairs
		Quat  q2       = getRotation(u0, u2);
		Vec3r v1       = rotated(v2, conjugate(q2));
		Vec3r v0_proj  = projectOntoPlane(v0, u0);
		Vec3r v1_proj  = projectOntoPlane(v1, u0);
		Quat  q1       = getRotation(v0_proj, v1_proj);
		return normalized(q2 * q1);
	}
}

#endif
