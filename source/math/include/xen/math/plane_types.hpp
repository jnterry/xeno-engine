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
	/// \brief Represents a plane by storing the values used by the
	/// plane equation ax + by + cz + d = 0
	/// where (a,b,c) is the normal to the plane
	template <typename T>
	struct PlaneEquation {
		/// \brief Normal to the plane
		Vec3<T> normal;

		/// \brief value of d in the plane equation
		T d;
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
}

#endif
