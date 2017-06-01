////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Angle.hpp
/// \author Jamie Terry
/// \date 2017/05/31
/// \brief Contains type for representing angles, helps in ensuring units are correct
/// and in converting between different units for angles
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_ANGLE_HPP
#define XEN_MATH_ANGLE_HPP

#include <xen/core/intrinsics.hpp>

#include <cmath>

namespace xen{
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
	#else
	inline float  sin (Angle a){ return sinf(a.radians); }
	inline float  cos (Angle a){ return cosf(a.radians); }
	inline float  tan (Angle a){ return tanf(a.radians); }

	inline Angle  asin(float a){ return { asinf(a) };    }
	inline Angle  acos(float a){ return { acosf(a) };    }
	inline Angle  atan(float a){ return { atanf(a) };    }
	#endif


	/// \brief Clamps an angle to being between 0 and 1 full revolution
	inline Angle clamp(Angle a){
		return { (real)fmod(a.radians, 2.0_r * xen::PI ) };
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

inline constexpr xen::Angle operator"" _deg(long double            val){ return xen::Degrees    ((real)val); }
inline constexpr xen::Angle operator"" _deg(unsigned long long int val){ return xen::Degrees    ((real)val); }

inline constexpr xen::Angle operator"" _rad(long double            val){ return xen::Radians    ((real)val); }
inline constexpr xen::Angle operator"" _rad(unsigned long long int val){ return xen::Radians    ((real)val); }

inline constexpr xen::Angle operator"" _rev(long double            val){ return xen::Revolutions((real)val); }
inline constexpr xen::Angle operator"" _rev(unsigned long long int val){ return xen::Revolutions((real)val); }


#endif
