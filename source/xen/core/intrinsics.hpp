////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains most basic helper functions and types used by the rest of the
/// engine
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_INTRINSICS_HPP
#define XEN_CORE_INTRINSICS_HPP

#include <stdint.h>
#include <cmath>
#include <cstdio>

#define XenArrayLength(array) (sizeof(array) / sizeof((array)[0]))

/// \brief Triggers break in debugger (or crash if no debugger)
/// \todo :TODO: something better?
#define XenBreak(...) { \
	printf("***XEN BREAK***\n"); \
	(*(char*)nullptr) = 'a'; \
	}

// :TODO: something better than printf?
#define XenAssert(cond, msg) \
	if(!(cond)){ \
		printf("*** XEN ASSERTION FAILURE: %s:%i - %s\n", __FILE__, __LINE__, msg); \
		XenBreak(__VAR_ARGS__); \
	}
#define XenInvalidCodePath(msg)	  \
	printf("***XEN INVALID CODE PATH: %s:%i - %s\n", __FILE__, __LINE__, msg); \
	XenBreak();

typedef float     r32;
typedef double    r64;
typedef int8_t    s8;
typedef int8_t    s08;
typedef int16_t   s16;
typedef int32_t   s32;
typedef int64_t   s64;
typedef uint8_t   u8;
typedef uint8_t   u08;
typedef uint16_t  u16;
typedef uint32_t  u32;
typedef uint64_t  u64;
typedef int8_t    b8;
typedef int32_t   b32;
typedef unsigned  int uint;
typedef uintptr_t uptr;
typedef  intptr_t sptr;

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

	/////////////////////////////////////////////////////////////////////
	/// \brief Finds the maximum of two or more items
	/////////////////////////////////////////////////////////////////////
	template<typename T> T max(const T a, const T b){ return a >  b ? a : b; }
	template<typename T> T max(const T a           ){ return a; }
	template<typename T, typename ...T_REST> T max(const T a, const T b, const T c, const T_REST... rest){
		return xen::max(xen::max(a, b), xen::max(c, rest...));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Finds the minimum of two or more items
	/////////////////////////////////////////////////////////////////////
	template<typename T> T min(const T a, const T b){ return a <= b ? a : b; }
	template<typename T> T min(const T a           ){ return a; }
	template<typename T, typename ...T_REST> T min(const T a, const T b, const T c, const T_REST... rest){
		return xen::min(xen::min(a, b), xen::min(c, rest...));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Swaps two values a and b
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	void swap(T& a, T& b){
		T tmp = a;
		a     = b;
		b     = tmp;
	}
}

inline constexpr real operator"" _r(long double            val){ return (real)val; }
inline constexpr real operator"" _r(unsigned long long int val){ return (real)val; }

#endif
