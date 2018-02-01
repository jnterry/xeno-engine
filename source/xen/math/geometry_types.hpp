////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file geometry_types.hpp
/// \author Jamie Terry
/// \date 2016/08/24
/// \brief Contains various type definitions for dealing with geometry
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_GEOMETRY_TYPES_HPP
#define XEN_MATH_GEOMETRY_TYPES_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/Vector.hpp>

namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Represents some finite line segment
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	struct LineSegment{
		Vec<T_DIM, T> p1;
		Vec<T_DIM, T> p2;
	};
	template <typename T> using LineSegment2 = LineSegment<2, T>;
	template <typename T> using LineSegment3 = LineSegment<3, T>;
	typedef LineSegment<2, real> LineSegment2r;
	typedef LineSegment<2, u32 > LineSegment2u;
	typedef LineSegment<2, s32 > LineSegment2s;
	typedef LineSegment<3, real> LineSegment3r;
	typedef LineSegment<3, u32 > LineSegment3u;
	typedef LineSegment<3, s32 > LineSegment3s;
	typedef LineSegment<4, real> LineSegment4r;
	typedef LineSegment<4, u32 > LineSegment4u;
	typedef LineSegment<4, s32 > LineSegment4s;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an Axis Aligned Bounding Box
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	struct Aabb{
		Vec<T_DIM, T> min;
		Vec<T_DIM, T> max;
	};
	typedef Aabb<2, u32>  Aabb2u;
	typedef Aabb<2, s32>  Aabb2s;
	typedef Aabb<2, real> Aabb2r;
	typedef Aabb<3, u32>  Aabb3u;
	typedef Aabb<3, s32>  Aabb3s;
	typedef Aabb<3, real> Aabb3r;
	template <typename T> using Aabb2 = Aabb<2, T>;
	template <typename T> using Aabb3 = Aabb<3, T>;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents sphere in some number of dimensions
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	struct Sphere{
		Vec<T_DIM, T> center;
		T             radius;
	};
	typedef Sphere<2, u32 > Sphere2u;
	typedef Sphere<2, s32 > Sphere2s;
	typedef Sphere<2, real> Sphere2r;
	typedef Sphere<3, u32 > Sphere3u;
	typedef Sphere<3, s32 > Sphere3s;
	typedef Sphere<3, real> Sphere3r;
	template <typename T> using Circle = Sphere<2, T>;


	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a triangle in some number of dimensions
	/////////////////////////////////////////////////////////////////////

	// gcc doesn't like the anonomous structures inside unions, disable the warning temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	template<u32 T_DIM, typename T>
	struct Triangle {
		union {
			struct {
				Vec<T_DIM, T> p1;
				Vec<T_DIM, T> p2;
				Vec<T_DIM, T> p3;
			};
			Vec<T_DIM, T> points[3];
		};
	};

	#pragma GCC diagnostic pop

	typedef Triangle<2, u32 > Triangle2u;
	typedef Triangle<2, s32 > Triangle2s;
	typedef Triangle<2, real> Triangle2r;
	typedef Triangle<3, u32 > Triangle3u;
	typedef Triangle<3, s32 > Triangle3s;
	typedef Triangle<3, real> Triangle3r;
	template <typename T> using Triangle2 = Triangle<2, T>;
	template <typename T> using Triangle3 = Triangle<3, T>;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a ray (aka half line) with some origin point
	/// and direction
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	struct Ray {
		/// \brief Position from which the ray is originating
		Vec<T_DIM, T> origin;

		/// \brief Direction vector (magnitude 1) representing where the ray is "pointing"
		Vec<T_DIM, T> direction;
	};
	typedef Ray<2, u32 > Ray2u;
	typedef Ray<2, s32 > Ray2s;
	typedef Ray<2, real> Ray2r;
	typedef Ray<3, u32 > Ray3u;
	typedef Ray<3, s32 > Ray3s;
	typedef Ray<3, real> Ray3r;
	template <typename T> using Ray2 = Ray<2, T>;
	template <typename T> using Ray3 = Ray<3, T>;

	/*template<u32 T_DIM, typename T>
	struct Line {
		/// \brief A point on the line
		Vec<T_DIM, T> point;

		/// \brief The direction vector of the line
		Vec<T_DIM, T> direction;
		};*/
}

template<u32 T_DIM, typename T>
bool operator==(const xen::Aabb<T_DIM, T>& lhs, const xen::Aabb<T_DIM, T>& rhs){
	return lhs.min == rhs.min && lhs.max == rhs.max;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Aabb<T_DIM, T>& lhs, const xen::Aabb<T_DIM, T>& rhs){
	return lhs.min != rhs.min || lhs.max != rhs.max;
}

template<u32 T_DIM, typename T>
bool operator==(const xen::LineSegment<T_DIM, T>& lhs, const xen::LineSegment<T_DIM, T>& rhs){
	return lhs.p1 == rhs.p1 && lhs.p2 == rhs.p2;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::LineSegment<T_DIM, T>& lhs, const xen::LineSegment<T_DIM, T>& rhs){
	return lhs.p1 != rhs.p1 || lhs.p2 != rhs.p2;
}

template<u32 T_DIM, typename T>
bool operator==(const xen::Sphere<T_DIM, T>& lhs, const xen::Sphere<T_DIM, T>& rhs){
	return lhs.center == rhs.center && lhs.radius == rhs.radius;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Sphere<T_DIM, T>& lhs, const xen::Sphere<T_DIM, T>& rhs){
	return lhs.center != rhs.center || lhs.radius != rhs.radius;
}

template<u32 T_DIM, typename T>
bool operator==(const xen::Triangle<T_DIM, T>& lhs, const xen::Triangle<T_DIM, T>& rhs){
	return (lhs.p1 == rhs.p1 &&
	        lhs.p2 == rhs.p2 &&
	        lhs.p3 == rhs.p3
	       );
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Triangle<T_DIM, T>& lhs, const xen::Triangle<T_DIM, T>& rhs){
	return (lhs.p1 != rhs.p1 ||
	        lhs.p2 != rhs.p2 ||
	        lhs.p3 != rhs.p3
	       );
}

template<u32 T_DIM, typename T>
bool operator==(const xen::Ray<T_DIM, T>& lhs, const xen::Ray<T_DIM, T>& rhs){
	return (lhs.origin == rhs.origin && lhs.direction == rhs.direction);
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Ray<T_DIM, T>& lhs, const xen::Ray<T_DIM, T>& rhs){
	return (lhs.origin != rhs.origin || lhs.direction != rhs.direction);
}

#endif
