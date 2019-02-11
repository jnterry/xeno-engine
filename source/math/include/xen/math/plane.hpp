////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for interacting with types declared in
/// plane_types.hpp
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_PLANE_HPP
#define XEN_MATH_PLANE_HPP

#include <xen/math/plane_types.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/utilities.hpp>

namespace xen {
	/// \brief Determines the distance between some point and a plane
	/// \note Return value is negative is the point is "behind" the plane
	/// (the normal points out from the front), and is positive if the
	/// point is in front of the plane -> use distanceToPlane for absolute
	/// value
	template<typename T>
	T getDirectionalDistanceToPlane(PlaneEquation<T> plane, Vec3<T> point){
		return xen::dot(plane.normal, point) + plane.d;
	}

	template<typename T>
	T distanceToPlane(PlaneEquation<T> plane, Vec3<T> point){
		return xen::abs(xen::getDirectionalDistanceToPlane(plane, point));
	}

	template<typename T>
	Vec3r getPlaneProjection(PlaneEquation<T> plane, Vec3<T> point){
		T dist = getDirectionalDistanceToPlane(plane, point);
		return point - dist * plane.normal;
	}
}

#endif
