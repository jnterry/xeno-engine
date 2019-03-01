////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains various type definitions for representing a 2d subspace
/// embedded within some 3d space
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_PLANE_TYPES_HPP
#define XEN_MATH_PLANE_TYPES_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/impl/vector_operations.hpp>

namespace xen {

	template<typename T> struct Plane;

	/// \brief Represents a plane by storing the values used by the
	/// plane equation ax + by + cz + d = 0
	/// where (a,b,c) is the normal to the plane
	template <typename T>
	struct PlaneEquation {
		/// \brief Normal to the plane
		Vec3<T> normal;

		/// \brief value of d in the plane equation
		T d;

		inline operator Vec4<T>&(){
			return *((Vec4<T>*)this);
		}
		inline operator const Vec4<T>&() const{
			return *((const Vec4<T>*)this);
		}
		operator Plane<T> () const;
	};

	/// \brief Represents a plane
	template <typename T>
	struct Plane {
		/// \brief The normal to the plane
		Vec3<T> normal;

		/// \brief some point on the plane
		Vec3<T> point;

		/// \brief Convert to PlaneEquation representation
		inline operator PlaneEquation<T>(){
			T d = (
				-this->point.x * normal.x
				-this->point.y * normal.y
				-this->point.z * normal.z
			);
			return { this->normal, d };
		}
	};
	typedef Plane<real> Plane3r;

	template<typename T>
	PlaneEquation<T>::operator Plane<T>() const {
		return { normal, normal * d };
	}

	/// \brief Represents a rectangular subsection of some 2d plane
	/// embedded in a 3d space
	///
	/// \note If the two edge vectors are not orthogonal to one another then
	/// this struct can instead represent a rhombus embedded within some plane
	template<typename T>
	struct PlaneRhombus {
		/// \brief One vertex of the rhombus
		Vec3<T> position;

		/// \brief Two edge vectors representing the offset from the position
		/// field by which two other vertices of the rhombus may be found
		///
		/// p + e[1] x----------x p + e[0] + e[1]
		///         /          /
		///        /          /
		///    p  x----------x p + e[0]
		Vec3<T> edge[2];



		inline explicit operator Plane<T>() {
			return {
				xen::normalized(xen::cross(this->edge[0], this->edge[1])), this->position
			};
		}

		inline explicit operator PlaneEquation<T>() { return (Plane<T>)(*this); }
	};
	typedef PlaneRhombus<real> PlaneRhombus3r;
}

#endif
