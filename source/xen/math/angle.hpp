////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains type for representing angles, helps in ensuring units are
/// correct and in converting between different units
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_ANGLE_HPP
#define XEN_MATH_ANGLE_HPP

#include <xen/core/intrinsics.hpp>

#include <cmath>

namespace xen{
	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an Angle in arbitrary dimensions.
	///
	/// The goal of this class if to make it possible to represent angles
	/// in whatever unit is natural to the problem/programmer while ensuring
	/// there are never any ambiguities about what the units are, and ensuring
	/// we never pass radians to a function expecting degrees, or vice-versa.
	///
	/// The data structure itself does nothing and has no overhead in terms of
	/// storage or speed as compared to using a scalar value to represent angles;
	/// instead it is the associated functions that provide this type with its
	/// power.
	///
	/// Angles can be expressed in any unit using the the named constructors
	/// Degrees, Radians or Revolutions, or by using the custom literal suffixes
	/// _deg, _rad or _rev.
	///
	/// By defining Angle as an actually type rather than just having conversion
	/// functions we also gain type safety to avoid some programming errors,
	/// for example, angle-angle multiplication is usually fairly meaningless
	/// (the units would be radians squared), but angle-scalar multiplication
	/// has meaning (simply changing the value of the angle). Conventions such
	/// as this can be enforced using operator overloading now that a new type
	/// has been defined to represent angles.
	/////////////////////////////////////////////////////////////////////
	struct Angle{
		real radians;
	};

	inline constexpr Angle Degrees    (real value){ return { (value * xen::PI) / 180.0_r }; }
	inline constexpr Angle Radians    (real value){ return { value                           }; }
	inline constexpr Angle Revolutions(real value){ return { value * 2.0_r * xen::PI     }; }

	inline real asDegrees   (Angle a){ return (a.radians * 180.0_r) / xen::PI; }
	inline real asRadians   (Angle a){ return a.radians;                           }
	inline real asRevolution(Angle a){ return a.radians / 2.0_r * xen::PI;     }

	#if XEN_USE_DOUBLE_PRECISION
	inline double sin (Angle a ){ return sin(a.radians); }
	inline double cos (Angle a ){ return cos(a.radians); }
	inline double tan (Angle a ){ return tan(a.radians); }

	inline Angle  asin(double a){ return { asin(a) };    }
	inline Angle  acos(double a){ return { acos(a) };    }
	inline Angle  atan(double a){ return { atan(a) };    }

	inline Angle  atan2(double a, double b){ return { atan2(a,b) }; }
	#else
	inline float  sin (Angle a){ return sinf(a.radians); }
	inline float  cos (Angle a){ return cosf(a.radians); }
	inline float  tan (Angle a){ return tanf(a.radians); }

	inline Angle  asin(float a){ return { asinf(a) };    }
	inline Angle  acos(float a){ return { acosf(a) };    }
	inline Angle  atan(float a){ return { atanf(a) };    }

	inline Angle  atan2(float a, float b){ return { atan2f(a,b) }; }
	#endif


	/// \brief Clamps an angle to being between 0 and 1 full revolution
	inline Angle clamp(Angle a){
		// clamp to between -1 and 1 revolution
		real val = (real)fmod(a.radians, 2.0_r * xen::PI );
		return { val < 0 ? val + 2.0_r * xen::PI : val  };
	}
}

inline xen::Angle operator+=(xen::Angle& lhs,  const xen::Angle& rhs){ lhs.radians += rhs.radians; return lhs; }
inline xen::Angle operator-=(xen::Angle& lhs,  const xen::Angle& rhs){ lhs.radians -= rhs.radians; return lhs; }
inline xen::Angle operator*=(xen::Angle& lhs,  real              rhs){ lhs.radians *= rhs;         return lhs; }
inline xen::Angle operator/=(xen::Angle& lhs,  real              rhs){ lhs.radians /= rhs;         return lhs; }

inline xen::Angle operator*(const xen::Angle& lhs, real              rhs){ return {lhs.radians * rhs}; }
inline xen::Angle operator*(real              lhs, const xen::Angle& rhs){ return {lhs * rhs.radians}; }
inline xen::Angle operator/(const xen::Angle& lhs, real              rhs){ return {lhs.radians / rhs}; }

inline xen::Angle operator+(const xen::Angle& lhs, const xen::Angle& rhs){ return {lhs.radians + rhs.radians}; }
inline xen::Angle operator-(const xen::Angle& lhs, const xen::Angle& rhs){ return {lhs.radians - rhs.radians}; }

inline xen::Angle operator-(const xen::Angle& a) { return { -a.radians }; }

inline bool operator==(const xen::Angle& a, const xen::Angle& b){
	real delta = a.radians - b.radians;
	return delta * delta <= 0.0000001;
}
inline bool operator!=(const xen::Angle& a, const xen::Angle& b){ return !(a == b); }
inline bool operator<=(const xen::Angle& a, const xen::Angle& b){ return a.radians <= b.radians; }
inline bool operator>=(const xen::Angle& a, const xen::Angle& b){ return a.radians >= b.radians; }
inline bool operator< (const xen::Angle& a, const xen::Angle& b){ return a.radians <  b.radians; }
inline bool operator> (const xen::Angle& a, const xen::Angle& b){ return a.radians >  b.radians; }

inline constexpr xen::Angle operator"" _deg(long double            val){ return xen::Degrees    ((real)val); }
inline constexpr xen::Angle operator"" _deg(unsigned long long int val){ return xen::Degrees    ((real)val); }

inline constexpr xen::Angle operator"" _rad(long double            val){ return xen::Radians    ((real)val); }
inline constexpr xen::Angle operator"" _rad(unsigned long long int val){ return xen::Radians    ((real)val); }

inline constexpr xen::Angle operator"" _rev(long double            val){ return xen::Revolutions((real)val); }
inline constexpr xen::Angle operator"" _rev(unsigned long long int val){ return xen::Revolutions((real)val); }


#endif
