////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for vectors upon the surface of the unit sphere
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_SPHERICAL_VECTOR_HPP
#define XEN_MATH_SPHERICAL_VECTOR_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/vector.hpp>

namespace xen {

	typedef Vec2<xen::Angle> LatLong;

	Vec3r toCartesian(const LatLong& v, real radius = 1){
		real cx = xen::cos(v.x);
		return radius * Vec3r{
			xen::cos(v.y) * cx,
			xen::sin(v.x),
			xen::sin(v.y) * -cx,
		};
	}
  LatLong toLatLong(Vec3r v){
	  v = xen::normalized(v);
		LatLong result;
		result.x = xen::asin(v.y);
		real cx  = xen::cos(result.x);
		result.y = xen::atan2(-v.z / cx, v.x / cx);
		return result;
	}
}

template<typename T>
xen::LatLong operator+(xen::LatLong& a, xen::LatLong& b){
	return { a.x + b.x, a.y + b.y };
}
template<typename T>
xen::LatLong operator-(const xen::LatLong& a, const xen::LatLong& b){
	return { a.x - b.x, a.y - b.y };
}

#endif
