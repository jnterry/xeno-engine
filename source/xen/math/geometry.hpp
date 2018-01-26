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

#include <xen/core/intrinsics.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/Matrix.hpp>

namespace xen{
	template<u32 T_DIM, typename T>
	Aabb<T_DIM, T>& translate(Aabb<T_DIM, T>& aabb, Vec<T_DIM, T> delta){
		aabb.min += delta;
		aabb.max += delta;
		return aabb;
	}

	Circle& translate(Circle& circle, Vec2r delta){
		circle.center += delta;
		return circle;
	}

	template<u32 T_DIM, typename T>
	LineSegment<T_DIM, T>& translate(LineSegment<T_DIM, T>& line, Vec<T_DIM, T> delta){
		line.p1 += delta;
		line.p2 += delta;
		return line;
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

	namespace impl{
		template<typename T>
		void movePointInto(Vec2r& point, Vec2r other_point, const Aabb2<T>& aabb){
			Vec2r dir = other_point - point;
			if(dir.x != 0){
				if(point.x < aabb.min.x && dir.x > 0){
					point += dir * ((aabb.min.x - point.x) / dir.x);
				}
				if(point.x > aabb.max.x && dir.x < 0){
					point += dir * ((aabb.max.x - point.x) / dir.x);
				}
			}
			if(dir.y != 0){
				if(point.y < aabb.min.y && dir.y > 0){
					point += dir * ((aabb.min.y - point.y) / dir.y);
				}
				if(point.y > aabb.max.y && dir.y < 0){
					point += dir * ((aabb.max.y - point.y) / dir.y);
				}
			}
		}
	}

	/// \brief clips a line segment such that it is containined within the specified aabb
	/// If the line does not intersect the aabb then sets both the lines end points to the origin
	template<typename T>
	LineSegment2<T>& intersect(LineSegment2<T>& line, Aabb2<T> aabb){
		if((line.p1.x < aabb.min.x && line.p2.x < aabb.min.x) ||
		   (line.p1.x > aabb.max.x && line.p2.x > aabb.max.x) ||
		   (line.p1.y < aabb.min.y && line.p2.y < aabb.min.y) ||
		   (line.p2.y > aabb.max.y && line.p2.y > aabb.max.y)){
			//then both points are beyond one side of the aabb, hence there is no intersection
			line.p1 = {0, 0};
			line.p2 = {0, 0};
			return line;
		} else {
			if(!contains(aabb, line.p1)){
				impl::movePointInto(line.p1, line.p2, aabb);
			}
			if(!contains(aabb, line.p2)){
				impl::movePointInto(line.p2, line.p1, aabb);
			}
			return line;
		}

	}

	template<typename T>
	Aabb2<T>& intersect(Aabb2<T>& a, const Aabb2<T> b){
		a.min.x = XenMax(a.min.x, b.min.x); //:TODO:COMP: vector max function which takes max of each axis?
		a.min.y = XenMax(a.min.y, b.min.y); //would also mean this function could be generic for Aabb2 and Aabb3
		a.max.x = XenMax(XenMin(a.max.x, b.max.x), b.min.x);
		a.max.y = XenMax(XenMin(a.max.y, b.max.y), b.min.y);
		return a;
	}

	template<typename T_LHS, typename T_RHS>
	T_LHS getIntersection(T_LHS lhs, T_RHS rhs){
		intersect(lhs, rhs);
		return lhs;
	}

	template<typename T>
	Aabb2<T> getIntersection(Aabb2<T> a, Aabb2<T> b){
		Aabb2<T> result = a;
		intersect(result, b);
		return result;
	}

	template<typename T>
	bool hasArea(Aabb2<T> aabb){
		return aabb.max.x > aabb.min.x && aabb.max.y > aabb.min.y;
	}
}

#endif
