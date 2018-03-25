////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for manipulating the types defined in
/// geometry_types.hpp
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_GEOMETRY_HPP
#define XEN_MATH_GEOMETRY_HPP

#include <xen/math/geometry_types.hpp>
#include <xen/math/vertex_group.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/utilities.hpp>
#include <xen/core/bits.hpp>
#include <xen/core/intrinsics.hpp>

#include "impl/swizzles.hxx"
#include "impl/geometry_aabb.hxx"

namespace xen{

	//////////////////////////////////////////////////////////////////////////////
	// Translate geometry type by some offset vector

	template<u32 T_DIM, typename T>
	Aabb<T_DIM, T>& translate(Aabb<T_DIM, T>& aabb, Vec<T_DIM, T> delta){
		aabb.min += delta;
		aabb.max += delta;
		return aabb;
	}

	template<u32 T_DIM, typename T>
	inline Sphere<T_DIM, T>& translate(Sphere<T_DIM, T>& sphere, Vec<T_DIM, T> delta){
		sphere.center += delta;
		return sphere;
	}

	template<u32 T_DIM, typename T>
  Ray<T_DIM, T>& translate(Ray<T_DIM, T>& ray, Vec<T_DIM, T> delta){
	  ray.origin += delta;
		return ray;
	}

	template<typename T_OBJ, typename T_VEC>
	T_OBJ getTranslated(T_OBJ original, T_VEC delta){
		translate(original, delta);
		return original;
	}

	//////////////////////////////////////////////////////////////////////////////
	// Transform geometry type by some arbitary matrix

	// Can't transform Aabb in general case, would result in arbitary quad

	// :TODO: transform sphere

	template<u32 T_DIM, typename T>
	Ray<T_DIM, T> transform(Ray<T_DIM, T>& ray, const xen::Matrix<T_DIM+1, T_DIM+1, T>& mat){
		Vec<T_DIM, T> point = ray.origin + ray.direction;

		ray.origin *= mat;
		point      *= mat;

		ray.direction = xen::normalized(point - ray.origin);

		return ray;
	}

	template<u32 T_DIM, typename T>
  Ray<T_DIM, T> getTransformed(const Ray<T_DIM, T>& original, const xen::Matrix<T_DIM+1, T_DIM+1, T>& mat){
		Ray<T_DIM, T> result = original;
		transform(result, mat);
		return result;
	}

	//////////////////////////////////////////////////////////////////////////////
	// Check if point lies within geometry

	/// \brief Checks if an Aabb contains a point
	/// \public \memberof Aabb
	template<typename T>
	bool contains(Aabb2<T> aabb, Vec2<T> point){
		return aabb.min.x <= point.x && point.x <= aabb.max.x &&
		       aabb.min.y <= point.y && point.y <= aabb.max.y;
	}

	/// \brief Checks if an Aabb contains a point
	/// \public \memberof Aabb
	template<typename T>
	bool contains(Aabb3<T> aabb, Vec3<T> point){
		return aabb.min.x <= point.x && point.x <= aabb.max.x &&
		       aabb.min.y <= point.y && point.y <= aabb.max.y &&
		       aabb.min.z <= point.z && point.z <= aabb.max.z;
	}

	/// \brief Checks if a Sphere or circle contains a point
	/// \public \memberof Aabb
	template<u32 T_DIM, typename T>
	bool contains(const Sphere<T_DIM, T>& sphere, const Vec<T_DIM, T>& point){
		return xen::distanceSq(sphere.center, point) <= (sphere.radius * sphere.radius);
	}

	//////////////////////////////////////////////////////////////////////////////

	/// \brief Clips a line segment such that it is fully contained within the
	/// specified Aabb.
	///
	/// If the line does not intersect the Aabb then returns false without
	/// modifying the passed in LineSegment2, else modifies the line segment
	/// such that it is fully contained by the Aabb2 and then returns true
	template<typename T>
	bool intersect(LineSegment2<T>& l, Aabb2<T> a){
		// https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm

		PointOutCode outcode_p1 = computePointOutCode(a, l.p1);
		PointOutCode outcode_p2 = computePointOutCode(a, l.p2);

		LineSegment2<T> r = l;

		while(outcode_p1 | outcode_p2) {
			if (outcode_p1 & outcode_p2) {
				// Both points share a common outside zone (left/right/etc)
				// so line must be outside of window
				return false;
			}

			T x, y;

			PointOutCode outcode_out = outcode_p1 ? outcode_p1 : outcode_p2;

			// Now find the intersection point;
			// use formulas:
			//   slope = (y1 - y0) / (x1 - x0)
			//   x = x0 + (1 / slope) * (ym - y0), where ym is ymin or ymax
			//   y = y0 + slope * (xm - x0), where xm is xmin or xmax
			// No need to worry about divide-by-zero because, in each case, the
			// outcode bit being tested guarantees the denominator is non-zero
			if (outcode_out & PointOutCode::UP) {
				real factor = (a.max.y - r.p1.y) / (r.p2.y - r.p1.y);
				x = r.p1.x + (r.p2.x - r.p1.x) * factor;
				y = a.max.y;
			} else if (outcode_out & PointOutCode::DOWN) {
				real factor = (a.min.y - r.p1.y) / (r.p2.y - r.p1.y);
				x = r.p1.x + (r.p2.x - r.p1.x) * factor;
				y = a.min.y;
			} else if (outcode_out & PointOutCode::RIGHT) {
				real factor = (a.max.x - r.p1.x) / (r.p2.x - r.p1.x);
				x = a.max.x;
				y = r.p1.y + (r.p2.y - r.p1.y) * factor;
			} else if (outcode_out & PointOutCode::LEFT) {
				real factor = (a.min.x - r.p1.x) / (r.p2.x - r.p1.x);
				x = a.min.x;
				y = r.p1.y + (r.p2.y - r.p1.y) * factor;
			}

			// Now we move outside point to intersection point to clip
			// and get ready for next pass.
			if (outcode_out == outcode_p1) {
				r.p1.x = x;
				r.p1.y = y;
				outcode_p1 = computePointOutCode(a, r.p1);
			} else {
				r.p2.x = x;
				r.p2.y = y;
				outcode_p2 = computePointOutCode(a, r.p2);
			}
		}

		l = r;
		return true;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Clips a line segment such that it is fully contained within the
	/// specified Aabb.
	///
	/// If the line does not intersect the Aabb then returns false without
	/// modifying the passed in LineSegment3, else modifies the line segment
	/// such that it is fully contained by the Aabb3 and then returns true
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	bool intersect(LineSegment3<T>& l, Aabb3<T> a){
		// https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm

		PointOutCode outcode_p1 = computePointOutCode(a, l.p1);
		PointOutCode outcode_p2 = computePointOutCode(a, l.p2);

		LineSegment3<T> r = l;

		while(outcode_p1 | outcode_p2) {
			if (outcode_p1 & outcode_p2) {
				// Both points share a common outside zone (left/right/etc)
				// so line must be outside Aabb
				return false;
			}

			T x, y, z;

			PointOutCode outcode_out = outcode_p1 ? outcode_p1 : outcode_p2;

			// Now find the intersection point;
			// use formulas:
			//   slope = (y1 - y0) / (x1 - x0)
			//   x = x0 + (1 / slope) * (ym - y0), where ym is ymin or ymax
			//   y = y0 + slope * (xm - x0), where xm is xmin or xmax
			// No need to worry about divide-by-zero because, in each case, the
			// outcode bit being tested guarantees the denominator is non-zero
			if (outcode_out & PointOutCode::UP) {
				real factor = (a.max.y - r.p1.y) / (r.p2.y - r.p1.y);
				x = r.p1.x + (r.p2.x - r.p1.x) * factor;
				y = a.max.y;
				z = r.p1.z + (r.p2.z - r.p1.z) * factor;
			} else if (outcode_out & PointOutCode::DOWN) {
				real factor = (a.min.y - r.p1.y) / (r.p2.y - r.p1.y);
				x = r.p1.x + (r.p2.x - r.p1.x) * factor;
				y = a.min.y;
				z = r.p1.z + (r.p2.z - r.p1.z) * factor;
			} else if (outcode_out & PointOutCode::RIGHT) {
				real factor = (a.max.x - r.p1.x) / (r.p2.x - r.p1.x);
				x = a.max.x;
				y = r.p1.y + (r.p2.y - r.p1.y) * factor;
				z = r.p1.z + (r.p2.z - r.p1.z) * factor;
			} else if (outcode_out & PointOutCode::LEFT) {
				real factor = (a.min.x - r.p1.x) / (r.p2.x - r.p1.x);
				x = a.min.x;
				y = r.p1.y + (r.p2.y - r.p1.y) * factor;
				z = r.p1.z + (r.p2.z - r.p1.z) * factor;
			} else if (outcode_out & PointOutCode::INFRONT) {
				real factor = (a.max.z - r.p1.z) / (r.p2.z - r.p1.z);
				x = r.p1.x + (r.p2.x - r.p1.x) * factor;
				y = r.p1.y + (r.p2.y - r.p1.y) * factor;
				z = a.max.z;
			} else if (outcode_out & PointOutCode::BEHIND) {
				real factor = (a.min.z - r.p1.z) / (r.p2.z - r.p1.z);
				x = r.p1.x + (r.p2.x - r.p1.x) * factor;
				y = r.p1.y + (r.p2.y - r.p1.y) * factor;
				z = a.min.z;
			}

			// Now we move outside point to intersection point to clip
			// and get ready for next pass.
			if (outcode_out == outcode_p1) {
				r.p1.x = x;
				r.p1.y = y;
				r.p1.z = z;
				outcode_p1 = computePointOutCode(a, r.p1);
			} else {
				r.p2.x = x;
				r.p2.y = y;
				r.p2.z = z;
				outcode_p2 = computePointOutCode(a, r.p2);
			}
		}

		l = r;
		return true;
	}

	template<typename T>
	bool haveIntersection(const Aabb2<T>& a, const Aabb2<T> b){
		return !(b.min.x > a.max.x ||
		         b.max.x < a.min.x ||
		         b.min.y > a.max.y ||
		         b.max.y < a.min.y);

	}

	template<u32 T_DIM, typename T>
	bool intersect(Aabb<T_DIM, T>& a, const Aabb<T_DIM, T> b){
		if(haveIntersection(a, b)){
			a.min = xen::max(a.min, b.min);
			a.max = xen::max(xen::min(a.max, b.max), b.min);
			return true;
		} else {
			a.min = Vec<T_DIM, T>::Origin;
			a.max = Vec<T_DIM, T>::Origin;
			return false;
		}
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the intersection between a ray and a triangle.
	/// \param ray The ray in question
	/// \param triangle The triangle in question
	/// \param result Vector in which the resulting point will be stored.
	/// Should the ray be in the same plane as the triangle, (hence causing the
	/// intersection to be a line segment) this will be set to the end of the
	/// line segment closest to the ray's origin. IE: the intersection point
	/// that the ray hits first.
	/// \return True if an intersection exists, else false. result will only be
	/// modified if true is returned
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	bool getIntersection(const Ray3<T>& ray, const Triangle3<T>& tri, Vec3<T>& result){
		// https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
		const real EPSILON = 0.0000001;
		Vec3<T> edge1, edge2, h, s, q;
		real a,f,u,v;
		edge1 = tri.p2 - tri.p1;
		edge2 = tri.p3 - tri.p1;
		h = xen::cross(ray.direction, edge2);
		a = xen::dot  (edge1, h);
		if (a > -EPSILON && a < EPSILON) {
			return false;
		}
		f = 1/a;
		s = ray.origin - tri.p1;
		u = f * (xen::dot(s, h));
		if (u < 0.0 || u > 1.0) {
			return false;
		}
		q = xen::cross(s, edge1);
		v = f * xen::dot(ray.direction, q);
		if (v < 0.0 || u + v > 1.0) {
			return false;
		}
		// At this stage we can compute t to find out where the intersection point is on the line.
		float t = f * xen::dot(edge2, q);
		if (t > EPSILON) {
			// ray intersection
			result = ray.origin + ray.direction * t;
			return true;
		} else {
			// This means that there is a line intersection but not a ray intersection.
			return false;
		}
	}

	template<typename T_LHS, typename T_RHS>
	T_LHS getIntersection(const T_LHS& lhs, const T_RHS& rhs){
		T_LHS result = lhs;
		intersect(result, rhs);
		return result;
	}

	template<typename T>
	bool hasArea(Aabb2<T> aabb){
		return aabb.max.x > aabb.min.x && aabb.max.y > aabb.min.y;
	}

	template<u32 T_DIM, typename T>
	bool hasArea(Sphere<T_DIM, T> sphere){
		return sphere.radius > 0;
	}

	template<u32 T_DIM, typename T>
	Aabb<T_DIM-1, T> fromHomo(Aabb<T_DIM, T> a){
		return Aabb<T_DIM-1, T>{ fromHomo(a.p1), fromHomo(a.p2) };
	}

	template<u32 T_DIM, typename T>
  Sphere<T_DIM-1, T> fromHomo(Sphere<T_DIM, T> a){
		return Sphere<T_DIM-1, T>{ fromHomo(a.center), a.radius };
	}

	template<u32 T_DIM, typename T>
	Aabb<T_DIM+1, T> toHomo(Aabb<T_DIM, T> a, T val = 1){
		return Aabb<T_DIM+1, T>{ toHomo(a.p1, val), toHomo(a.p2, val) };
	}

	template<u32 T_DIM, typename T>
	Sphere<T_DIM+1, T> toHomo(Sphere<T_DIM, T> a, T val = 1){
		return Sphere<T_DIM+1, T>{ toHomo(a.center, val), a.radius };
	}

	//////////////////////////////////////////////////////////////////////////////
	// Swizzles

	template<char T_S1, char T_S2, u32 T_DIM, typename T>
  Aabb2<T> swizzle(const Aabb<T_DIM, T>& a){
		return {
			xen::swizzle<T_S1, T_S2>(a.min),
			xen::swizzle<T_S1, T_S2>(a.max),
		};
	}
	template<char T_S1, char T_S2, char T_S3, u32 T_DIM, typename T>
  Aabb3<T> swizzle(const Aabb<T_DIM, T>& a){
		return {
			xen::swizzle<T_S1, T_S2, T_S3>(a.min),
			xen::swizzle<T_S1, T_S2, T_S3>(a.max),
		};
	}
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_DIM, typename T>
  Aabb4<T> swizzle(const Aabb<T_DIM, T>& a){
		return {
			xen::swizzle<T_S1, T_S2, T_S3, T_S4>(a.min),
			xen::swizzle<T_S1, T_S2, T_S3, T_S4>(a.max),
		};
	}

	template<char T_S1, char T_S2, u32 T_DIM, typename T>
	Circle<T> swizzle(const Sphere<T_DIM, T>& s){
		return { xen::swizzle<T_S1, T_S2>(s.center), s.radius };
	}
	template<char T_S1, char T_S2, char T_S3, u32 T_DIM, typename T>
  Sphere3<T> swizzle(const Sphere<T_DIM, T>& s){
		return { xen::swizzle<T_S1, T_S2, T_S3>(s.center), s.radius };
	}
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_DIM, typename T>
  Sphere4<T> swizzle(const Sphere<T_DIM, T>& s){
		return { xen::swizzle<T_S1, T_S2, T_S3, T_S4>(s.center), s.radius };
	}

	template<char T_S1, char T_S2, u32 T_DIM, typename T>
  Ray2<T> swizzle(const Ray<T_DIM, T>& r){
		return {
			xen::swizzle<T_S1, T_S2>(r.origin   ),
			xen::swizzle<T_S1, T_S2>(r.direction),
		};
	}
	template<char T_S1, char T_S2, char T_S3, u32 T_DIM, typename T>
  Ray3<T> swizzle(const Ray<T_DIM, T>& r){
		return {
			xen::swizzle<T_S1, T_S2, T_S3>(r.origin   ),
			xen::swizzle<T_S1, T_S2, T_S3>(r.direction),
		};
	}
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_DIM, typename T>
  Ray4<T> swizzle(const Ray<T_DIM, T>& r){
		return {
			xen::swizzle<T_S1, T_S2, T_S3, T_S4>(r.origin   ),
			xen::swizzle<T_S1, T_S2, T_S3, T_S4>(r.direction),
		};
	}
}

#endif
