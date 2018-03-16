////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Defines the Quaternion and AxisAngle types
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_QUATERNION_TYPES_HPP
#define XEN_MATH_QUATERNION_TYPES_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/vector_types.hpp>

////////////////////////////////////////////////////////////////////////////////
// Define the types
namespace xen{

	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	struct Quaternion;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a rotation as an axis about which to rotate,
	/// and an angle by which we will rotate in the clockwise direction
	/// when looking along the axis
	/////////////////////////////////////////////////////////////////////
	struct AxisAngle{
		AxisAngle() {}
		AxisAngle(Vec3r naxis, Angle nangle) : axis(naxis), angle(nangle) {}
		AxisAngle(Quaternion q);
		Vec3r axis;
		Angle angle;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a quaternion, coefficents stored in i,j,k, real order
	/////////////////////////////////////////////////////////////////////
	struct Quaternion{
		Quaternion() {}
		Quaternion(real nx, real ny, real nz, real nw) : x(nx), y(ny), z(nz), w(nw) {}
		Quaternion(Vec3r axis, Angle a);
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

	#pragma GCC diagnostic pop // re-enable -Wpedantic
}

typedef xen::Quaternion Quat;

inline bool operator==(xen::Quaternion l, xen::Quaternion r){ return l.xyzw == r.xyzw; }
inline bool operator!=(xen::Quaternion l, xen::Quaternion r){ return l.xyzw != r.xyzw; }
inline bool operator==(xen::AxisAngle  l, xen::AxisAngle  r){
	return l.axis == r.axis && l.angle == r.angle;
}
inline bool operator!=(xen::AxisAngle  l, xen::AxisAngle  r){
	return l.axis != r.axis || l.angle != r.angle;
}

#endif
