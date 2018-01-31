////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file geometry.hpp
/// \author Jamie Terry
/// \date 2016/08/24
/// \brief Contains functions for manipulating the types defined in geometry.hpp
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_GEOMETRY_HPP
#define XEN_MATH_GEOMETRY_HPP

#include <cstdio>

#include <xen/core/intrinsics.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/utilities.hpp>

namespace xen{
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
	LineSegment<T_DIM, T>& translate(LineSegment<T_DIM, T>& line, Vec<T_DIM, T> delta){
		line.p1 += delta;
		line.p2 += delta;
		return line;
	}

	template<u32 T_DIM, typename T>
  Triangle<T_DIM, T>& translate(Triangle<T_DIM, T>& triangle, Vec<T_DIM, T> delta){
		triangle.p1 += delta;
		triangle.p2 += delta;
		triangle.p3 += delta;
		return triangle;
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

	template<u32 T_DIM, typename T>
	LineSegment<T_DIM, T> transform(LineSegment<T_DIM, T>& line, xen::Matrix<T_DIM+1, T_DIM+1, T> mat){
		line.p1 *= mat;
		line.p2 *= mat;
		return line;
	}

	template<u32 T_DIM, typename T>
	Triangle<T_DIM, T> transform(Triangle<T_DIM, T>& triangle, xen::Matrix<T_DIM+1, T_DIM+1, T> mat){
		triangle.p1 *= mat;
		triangle.p2 *= mat;
		triangle.p3 *= mat;
		return triangle;
	}

	template<u32 T_DIM, typename T>
	Ray<T_DIM, T> transform(Ray<T_DIM, T>& ray, xen::Matrix<T_DIM+1, T_DIM+1, T> mat){
		ray.origin    *= mat;
		ray.direction  = xen::normalized(ray.direction * mat);
		return ray;
	}

	template<typename T_OBJ, typename T_MAT>
	T_OBJ getTransformed(const T_OBJ& original, T_MAT mat){
		T_OBJ result = original;
		transform(result, mat);
		return result;
	}

	template<typename T>
	bool contains(Aabb2<T> aabb, Vec2<T> point){
		return aabb.min.x <= point.x && point.x <= aabb.max.x &&
		       aabb.min.y <= point.y && point.y <= aabb.max.y;
	}

	template<typename T>
	bool contains(Aabb3<T> aabb, Vec3<T> point){
		return aabb.min.x <= point.x && point.x <= aabb.max.x &&
		       aabb.min.y <= point.y && point.y <= aabb.max.y &&
		       aabb.min.z <= point.z && point.z <= aabb.max.z;
	}

	template<u32 T_DIM, typename T>
	bool contains(const Sphere<T_DIM, T>& sphere, const Vec<T_DIM, T>& point){
		return xen::distance(sphere.center, point) <= sphere.radius;
	}

	namespace impl{
		template<u32 T_DIM, typename T>
		void movePointInto(Vec<T_DIM, T>& point, const Vec<T_DIM, T>& other_point, const Aabb<T_DIM, T>& aabb){
			Vec<T_DIM, T> dir = other_point - point;

			for(u32 dim = 0; dim < T_DIM; ++dim){
				if(dir[dim] != 0){
					if(point[dim] < aabb.min[dim] && dir[dim] > 0){
						point += dir * ((aabb.min[dim] - point[dim]) / dir[dim]);
					}
					if(point[dim] > aabb.max[dim] && dir[dim] < 0){
						point += dir * ((aabb.max[dim] - point[dim]) / dir[dim]);
					}
				}
			}
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Code representing the position of a point compared to an Aabb
		/// see: https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
		/////////////////////////////////////////////////////////////////////
		enum PointOutCode : u08{
			/// \brief Point is inside the Aabb
			INSIDE    = 0,

			/// \brief Point is to the left of the Aabb (x too small)
			LEFT      = 1,

			/// \brief Point is to the left of the Aabb (x too big)
			RIGHT     = 2,

			/// \brief Point is below of the Aabb (y too small)
			DOWN      = 4,

			/// \brief Point is above of the Aabb (y too big)
			UP        = 8,

			/// \brief Point is behind the Aabb (z too small)
			BACKWARDS = 16,

			/// \brief Point is in front of the Aabb (z too big)
			FORWARDS  = 32,
		};

		template <typename T>
		u08 computePointOutCode(Aabb2<T> a, Vec2<T> p){
			u08 result = PointOutCode::INSIDE;

			if(p.x < a.min.x){ result |= PointOutCode::LEFT;  }
			if(p.x > a.max.x){ result |= PointOutCode::RIGHT; }
			if(p.y < a.min.y){ result |= PointOutCode::DOWN;  }
			if(p.y > a.max.y){ result |= PointOutCode::UP;    }

			return result;
		}

		template <typename T>
		u08 computePointOutCode(Aabb3<T> a, Vec3<T> p){
			u08 result = PointOutCode::INSIDE;

			if(p.x < a.min.x){ result |= PointOutCode::LEFT;      }
			if(p.x > a.max.x){ result |= PointOutCode::RIGHT;     }
			if(p.y < a.min.y){ result |= PointOutCode::DOWN;      }
			if(p.y > a.max.y){ result |= PointOutCode::UP;        }
			if(p.z < a.min.z){ result |= PointOutCode::BACKWARDS; }
			if(p.z > a.max.z){ result |= PointOutCode::FORWARDS;  }

			return result;
		}
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines if there exists an intersection between some Aabb
	/// and a line segment
	/////////////////////////////////////////////////////////////////////
	//template<typename T>
	//bool haveIntersection(const LineSegment<T_DIM, T>& l, const Aabb<T_DIM, T>& a){
		//
		//		LineSegment<T_DIM, T>& l,
			 //	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Clips a line segment such that it is fully contained within the
	/// specified Aabb. If the line does not intersect the Aabb then sets both
	/// the line end points to the origin
	/// \return True if an intersection was found, false if no such
	/// intersection exists
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	bool intersect(LineSegment2<T>& l, Aabb2<T> a){
		// https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm

		int outcode_p1 = impl::computePointOutCode(a, l.p1);
		int outcode_p2 = impl::computePointOutCode(a, l.p2);

		bool accept = false;

		LineSegment2<T> r = l;

		while(true) {
			if(!(outcode_p1 | outcode_p2)) {
				accept = true;
				break;
			} else if (outcode_p1 & outcode_p2) {
				// Both points share a common outside zone (left/right/etc)
				// so line must be outside of window
				break;
			} else {
				T x, y;

				u08 outcode_out = outcode_p1 ? outcode_p1 : outcode_p2;

				// Now find the intersection point;
				// use formulas:
				//   slope = (y1 - y0) / (x1 - x0)
				//   x = x0 + (1 / slope) * (ym - y0), where ym is ymin or ymax
				//   y = y0 + slope * (xm - x0), where xm is xmin or xmax
				// No need to worry about divide-by-zero because, in each case, the
				// outcode bit being tested guarantees the denominator is non-zero
				if (outcode_out & impl::PointOutCode::UP) {
					x = r.p1.x + (r.p2.x - r.p1.x) * (a.max.y - r.p1.y) / (r.p2.y - r.p1.y);
					y = a.max.y;
				} else if (outcode_out & impl::PointOutCode::DOWN) {
					x = r.p1.x + (r.p2.x - r.p1.x) * (a.min.y - r.p1.y) / (r.p2.y - r.p1.y);
					y = a.min.y;
				} else if (outcode_out & impl::PointOutCode::RIGHT) {
					y = r.p1.y + (r.p2.y - r.p1.y) * (a.max.x - r.p1.x) / (r.p2.x - r.p1.x);
					x = a.max.x;
				} else if (outcode_out & impl::PointOutCode::LEFT) {
					y = r.p1.y + (r.p2.y - r.p1.y) * (a.min.x - r.p1.x) / (r.p2.x - r.p1.x);
					x = a.min.x;
				}

				// Now we move outside point to intersection point to clip
				// and get ready for next pass.
				if (outcode_out == outcode_p1) {
					r.p1.x = x;
					r.p1.y = y;
					outcode_p1 = impl::computePointOutCode(a, r.p1);
				} else {
					r.p2.x = x;
					r.p2.y = y;
					outcode_p2 = impl::computePointOutCode(a, r.p2);
				}
			}
		}

		if(accept) {
			l = r;
			return true;
		} else {
			l.p1 = Vec2<T>::Origin;
			l.p2 = Vec2<T>::Origin;
			return false;
		}
	}

	template<typename T>
	bool haveIntersection(const Aabb2<T>& a, const Aabb2<T> b){
		return !(b.min.x > a.max.x ||
		         b.max.x < a.min.x ||
		         b.min.y > a.max.y ||
		         b.max.y < a.min.y);

	}

	template<typename T>
  bool intersect(Aabb2<T>& a, const Aabb2<T> b){
		if(haveIntersection(a, b)){
			a.min.x = XenMax(a.min.x, b.min.x); //:TODO:COMP: vector max function which takes max of each axis?
			a.min.y = XenMax(a.min.y, b.min.y); //would also mean this function could be generic for Aabb2 and Aabb3
			a.max.x = XenMax(XenMin(a.max.x, b.max.x), b.min.x);
			a.max.y = XenMax(XenMin(a.max.y, b.max.y), b.min.y);
			return true;
		} else {
			a.min.x = 0;
			a.min.y = 0;
			a.max.x = 0;
			a.max.y = 0;
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
}

#endif
