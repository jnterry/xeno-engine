////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for vectors upon the surface of the unit sphere
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_LATLONG_HPP
#define XEN_MATH_LATLONG_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/angle.hpp>

namespace xen {

	typedef Vec2<xen::Angle> LatLong;

	inline Vec3r toCartesian(const LatLong& v, real radius = 1){
		real cx = xen::cos(v.x);
		return radius * Vec3r{
			xen::cos(v.y) * cx,
			xen::sin(v.x),
			xen::sin(v.y) * -cx,
		};
	}
  inline LatLong toLatLong(Vec3r v){
	  v = xen::normalized(v);
		LatLong result;
		result.x = xen::asin(v.y);
		real cx  = xen::cos(result.x);
		result.y = xen::atan2(-v.z / cx, v.x / cx);
		return result;
	}

	/// \brief Clamps a lat long into a range such that there exists a unique
	/// representation for any point on the surface of the sphere
	/// lat  (x) becomes between -90_deg and 90_deg,
	/// long (y) becomes between -180_deg and 179.99999_deg
	inline LatLong clamp(LatLong ll){
		// Clamp lat to between -180 and 180
		xen::Angle lat = xen::clamp(ll.x + 180_deg) - 180_deg;
		xen::Angle lon = ll.y;

		// If |lat| is greater than 90, then spin 180_deg around the equator,
		// and change, eg, 95deg -> 85deg to reflect min distance from equator
		if(lat < -90_deg){
			lat  = -180_deg - lat;
			lon += 180_deg;
		} else if (lat > 90_deg){
			lat  = 180_deg - lat;
			lon += 180_deg;
		}

		return { lat, xen::clamp(lon + 180_deg) - 180_deg };
	}
}

template<typename T>
inline xen::LatLong operator+(xen::LatLong& a, xen::LatLong& b){
	return { a.x + b.x, a.y + b.y };
}
template<typename T>
inline xen::LatLong operator-(const xen::LatLong& a, const xen::LatLong& b){
	return { a.x - b.x, a.y - b.y };
}

#endif
