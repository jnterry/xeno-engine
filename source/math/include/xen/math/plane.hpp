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

	/// \brief Computes the point within or on the boundary of some PlaneRhombus
	/// which has the smallest distance to some other point
	template<typename T>
	Vec3<T> getClosestPoint(PlaneRhombus<T> rhom, Vec3<T> point){
		Vec3r k = getPlaneProjection((PlaneEquation<T>)rhom, point);
		Vec3r p = rhom.position, e0 = rhom.edge[0], e1 = rhom.edge[1];

		// We simply need to solve simultaneous equations given by:
		// k = p + ae[0] + be[1]
		// to find a and b
		// 3 equations (since x,y and z coord) with two unknowns (a and b)
		// then we clamp a and b to be between 0 and 1 to ensure the returned point
		// is within the rhombus

		T a = (k.x * e1.y + k.y * e1.x + p.y * (e1.x + e1.y)) / (e0.x * e1.y - e0.y * e1.x);
		T b = (k.y + a*e0.y + p.y) / e1.y;

		a = xen::clamp(a, 0_r, 1_r);
		b = xen::clamp(b, 0_r, 1_r);

		return p + a*e0 + b*e1;
	}

	template<typename T>
	T getClosestPointDistanceSq(PlaneRhombus<T> prect, Vec3<T> point){
		Vec3<T> p = getClosestPoint(prect, point);
		return xen::distanceSq(p, point);
	}

	template<typename T>
	T getClosestPointDistance(PlaneRhombus<T> prect, Vec3<T> point){
		Vec3<T> p = getClosestPoint(prect, point);
		return xen::distance(p, point);
	}
}

#endif
