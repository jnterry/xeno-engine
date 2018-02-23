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
#include <xen/math/vector_types.hpp>

namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an Axis Aligned Bounding Box
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	struct Aabb{
		Vec<T_DIM, T> min;
		Vec<T_DIM, T> max;

		template<typename T2>
		explicit operator Aabb<T_DIM, T2>() const {
			return { (Vec<T_DIM, T2>)min, (Vec<T_DIM, T2>)max };
		}
	};
	template <typename T> using Aabb2 = Aabb<2, T>;
	template <typename T> using Aabb3 = Aabb<3, T>;
	template <typename T> using Aabb4 = Aabb<4, T>;
	typedef Aabb<2, u32>  Aabb2u;
	typedef Aabb<2, s32>  Aabb2s;
	typedef Aabb<2, real> Aabb2r;
	typedef Aabb<3, u32>  Aabb3u;
	typedef Aabb<3, s32>  Aabb3s;
	typedef Aabb<3, real> Aabb3r;
	typedef Aabb<4, u32>  Aabb4u;
	typedef Aabb<4, s32>  Aabb4s;
	typedef Aabb<4, real> Aabb4r;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents sphere in some number of dimensions
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	struct Sphere{
		Vec<T_DIM, T> center;
		T             radius;
	};
	template <typename T> using Circle  = Sphere<2, T>;
	template <typename T> using Sphere3 = Sphere<3, T>;
	template <typename T> using Sphere4 = Sphere<4, T>;
	typedef Sphere<2, u32 > Sphere2u;
	typedef Sphere<2, s32 > Sphere2s;
	typedef Sphere<2, real> Sphere2r;
	typedef Sphere<3, u32 > Sphere3u;
	typedef Sphere<3, s32 > Sphere3s;
	typedef Sphere<3, real> Sphere3r;
	typedef Sphere<4, u32 > Sphere4u;
	typedef Sphere<4, s32 > Sphere4s;
	typedef Sphere<4, real> Sphere4r;

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
	template <typename T> using Ray2 = Ray<2, T>;
	template <typename T> using Ray3 = Ray<3, T>;
	template <typename T> using Ray4 = Ray<4, T>;
	typedef Ray<2, u32 > Ray2u;
	typedef Ray<2, s32 > Ray2s;
	typedef Ray<2, real> Ray2r;
	typedef Ray<3, u32 > Ray3u;
	typedef Ray<3, s32 > Ray3s;
	typedef Ray<3, real> Ray3r;
	typedef Ray<4, u32 > Ray4u;
	typedef Ray<4, s32 > Ray4s;
	typedef Ray<4, real> Ray4r;

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
bool operator==(const xen::Sphere<T_DIM, T>& lhs, const xen::Sphere<T_DIM, T>& rhs){
	return lhs.center == rhs.center && lhs.radius == rhs.radius;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Sphere<T_DIM, T>& lhs, const xen::Sphere<T_DIM, T>& rhs){
	return lhs.center != rhs.center || lhs.radius != rhs.radius;
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
