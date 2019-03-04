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
#include <xen/math/geometry_types.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/matrix.hpp>

namespace xen {
	/// \brief Determines the distance between some point and a plane
	/// \note Return value is negative is the point is "behind" the plane
	/// (the normal points out from the front), and is positive if the
	/// point is in front of the plane -> use distanceToPlane for absolute
	/// value
	template<typename T>
	T getSignedDistanceToPlane(PlaneEquation<T> plane, Vec3<T> point){
		return xen::dot(plane.normal, point) + plane.d;
	}

	template<typename T>
	T distanceToPlane(PlaneEquation<T> plane, Vec3<T> point){
		return xen::abs(xen::getSignedDistanceToPlane(plane, point));
	}

	template<typename T>
	Vec3r getPlaneProjection(PlaneEquation<T> plane, Vec3<T> point){
		T dist = getSignedDistanceToPlane(plane, point);
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

	/////////////////////////////////////////////////////////////////////
	/// \brief Finds the intersection between some direction vector and a plane
	/// Returns true if such an intersection exists, else returns false
	/// Result is only modified when true is set
	/// \note If the ray's direction is orthogonal to the plane's normal
	/// then the ray is said to be parallel with the plane. In this case this
	/// function will return false and result will not be modified. Note that
	/// this behaviour will occur even when the ray's origin itself lies within
	/// the plane!
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	bool getIntersection(const Plane<T>& plane, Ray3<T> ray, Vec3<T>& result){
		Vec3<T> diff = plane.point - ray.origin;
		T d = xen::dot(plane.normal, diff);
		T e = xen::dot(plane.normal, ray.direction);
		if(e){
			T t = d / e;
			if(t < 0){
				// Then intersection is behind the origin
				return false;
			} else {
				result = ray.origin + ray.direction * t;
				return true;
			}
		  return true;
		} else {
			// Either ray is in the plane or it is parallel
			return false;
		}
	}

	template<typename T>
	PlaneEquation<T> getTransformed(const PlaneEquation<T>& plane, const Mat4<T> mat){
		Vec4<T> v = xen::toHomo(plane.normal, plane.d);
		v = v * xen::transposed(xen::getInverse(mat));

		T len = xen::length(v.xyz);

		return { v.xyz / len, v.w / len };
		//Plane<T> result = plane;
		//result = getTransformed(result, mat);
		//return result;
	}
}

#endif
