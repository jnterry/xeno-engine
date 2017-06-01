////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file intrinsics.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains most basic helper functions and types used by the rest of the
/// engine
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_INTRINSICS_HPP
#define XEN_CORE_INTRINSICS_HPP

#include <stdint.h>
#include <cmath>

#define XenArrayLength(array) (sizeof(array) / sizeof((array)[0]))

#define XenMin(A, B) ((A < B) ? (A) : (B))
#define XenMax(A, B) ((A < B) ? (B) : (A))

/// \brief Triggers break in debugger (or crash if no debugger)
/// \todo :TODO: something better?
#define XenBreak(...) (*(char*)nullptr) = 'a';

#define XenAssert(cond, ...) { if(!(cond)){ XenBreak(__VAR_ARGS__); } }

typedef float    r32;
typedef double   r64;
typedef int8_t   s8;
typedef int8_t   s08;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;
typedef uint8_t  u8;
typedef uint8_t  u08;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   b8;
typedef int32_t  b32;
typedef unsigned int uint;

#ifdef XEN_USE_DOUBLE_PRECISION
	typedef double real;
#else
	typedef float  real;
#endif

namespace xen{
	/// \brief Base class for types which should not be able to be copied
	struct NonCopyable{
		NonCopyable(){}
	private:
		NonCopyable(const NonCopyable& other)            = delete;
		NonCopyable& operator=(const NonCopyable& other) = delete;
	};

	static const constexpr real PI = (real)M_PI;

	#ifdef XEN_USE_DOUBLE_PRECISION
	inline double sqrt(double val) { return sqrt(val);  }
	#else
	inline float  sqrt(float  val) { return sqrtf(val); }
	#endif

	template<typename T>
	T sign(T a){ return (a < 0) ? -1 : 1; }
}

inline constexpr real operator"" _r(long double            val){ return (real)val; }
inline constexpr real operator"" _r(unsigned long long int val){ return (real)val; }

#endif
