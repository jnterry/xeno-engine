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

	/// \brief Maps points on a cube with min and max verts of { -1, -1, -1 }
	/// and { 1, 1, 1 } respectively to a corresponding point on unit sphere
	///
	/// A naive but conceptually easy method would be to simply normalise the
	/// point on the unit cube such that it is at unit length from the sphere,
	/// however this function uses a slightly more expensive projection in order
	/// to try and reduce the stretching at the corners of the cube's faces
	inline Vec3r projectCubeToSphere(Vec3r p){
		// http://mathproofs.blogspot.com/2005/07/mapping-cube-to-sphere.html

		real x2 = p.x * p.x;
		real y2 = p.y * p.y;
		real z2 = p.z * p.z;

		p.x *= xen::sqrt(1 - (y2/2.0) - (z2/2.0) + ((y2*z2)/3.0));
		p.y *= xen::sqrt(1 - (z2/2.0) - (x2/2.0) + ((z2*x2)/3.0));
		p.z *= xen::sqrt(1 - (x2/2.0) - (y2/2.0) + ((x2*y2)/3.0));

		return p;
	}

	/// \brief Maps a point on the surface of a unit sphere to a point
	/// on a face of a cube with min and max verts of { -1, -1, -1 } and { 1, 1, 1 }
	/// respectively
	inline Vec3r projectSphereToCube(Vec3r p){
		// The important observation here is that p is a direction vector which
		// we want to extend in length such that it lies on the surface of the
		// cube. In order for a point to lie on the cube it must satisfy:
		// - One or more components must have the value of +1 or -1
		// - All other components must be in the range -1 -> 1 inclusive
		//
		// Hence we can force the point onto the cube by finding the component
		// with the current maximum value, and multiplying the entire direction
		// vector by some factor to make that component equal -1 or +1 such
		// that we satisfy the conditions above
		u32 max_comp_idx;
		Vec3r p_abs = xen::abs(p);

		if(p_abs.x > p_abs.y && p_abs.x > p_abs.z){
			max_comp_idx = 0;
		} else if (p_abs.y > p_abs.z){
			max_comp_idx = 1;
		} else {
			max_comp_idx = 2;
		}

		return p * (1.0 / p_abs[max_comp_idx]);
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
