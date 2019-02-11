////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for representing viewing frustrum
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_FRUSTRUM_TYPES_HPP
#define XEN_MATH_FRUSTRUM_TYPES_HPP

#include "plane_types.hpp"

namespace xen {
	/// \brief Represents a viewing frustrum
	template<typename T>
	struct Frustrum {
		/// \brief Enumeration plane indices
		enum Plane {
			Left   = 0,
			Right  = 1,
			Top    = 2,
			Bottom = 3,
			Near   = 4,
			Far    = 5,
		};

		/// \brief The planes making up the frustum.
		/// \note Plane normals point INTO the frustrum
		PlaneEquation<T> plane[6];
	};

	typedef Frustrum<real> Frustrum3r;
}

#endif
