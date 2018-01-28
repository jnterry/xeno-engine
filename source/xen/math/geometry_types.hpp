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
	typedef LineSegment<3, real> LineSegment3r;

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
	/// \brief Represents a 2d circle
	/////////////////////////////////////////////////////////////////////
	struct Circle{
		Vec2r center;
		real  radius;
	};
}

template<u32 T_DIM, typename T>
bool operator==(const xen::Aabb<T_DIM, T>& lhs, const xen::Aabb<T_DIM, T>& rhs){
	return lhs.min == rhs.min && lhs.max == rhs.max;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Aabb<T_DIM, T>& lhs, const xen::Aabb<T_DIM, T>& rhs){
	return lhs.min != rhs.min || lhs.max != rhs.max;
}

#endif
