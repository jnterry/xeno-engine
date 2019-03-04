////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains most basic helper functions, macros  and types used by the
/// rest of the engine
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_INTRINSICS_HPP
#define XEN_CORE_INTRINSICS_HPP

#include <cfloat>
#include <cstdint>
#include <cmath>
#include <cstdio>

////////////////////////////////////////////////////////////////////////////////
// * Misc Macros

/// \brief Helper macro which can determine the length of a compile time
/// fixed array. For example:
/// int thing[5];
/// XenArrayLength(thing); // will evaluate to 5
#define XenArrayLength(array) (sizeof(array) / sizeof((array)[0]))

/// \brief Triggers break in debugger (or crash if no debugger)
/// \todo :TODO: something better than crash -> ideally generate stack trace
#define XenBreak(...) { \
		printf("*** XEN BREAK %s:%i\n", __FILE__, __LINE__); \
		(*(char*)nullptr) = 'a'; \
	}

/// \brief Causes a XenBreak if some condition does not hold.
/// Active in all build types. Use XenDebugAssert for assertions
/// which are too costly at runtime to enable in production builds
///
/// \see XenDebugAssert
///
/// \todo :TODO: something better than printf?
/// This is effectively log fatal but with with a check built in
#define XenAssert(cond, msg) \
	if(!(cond)){ \
		printf("*** XEN ASSERTION FAILURE: %s:%i - %s\n", __FILE__, __LINE__, msg); \
		XenBreak(__VAR_ARGS__); \
	}

#if XEN_DEBUG_CHECKS
	/// \brief Behaves as XenAssert when XEN_DEBUG_CHECKS is defined,
	/// otherwise is defined to be a no-op
	/// \see XenAssert
	#define XenDebugAssert(cond, msg) { XenAssert(cond, msg) }
#else
	#define XenDebugAssert(cond, msg)
#endif

/// \brief Wrapper around XenBreak which indicates an invalid code path
/// was taken. Usually used as a default case in a switch, or a in an else
/// after a series of else ifs to indicate some condition occurred that
/// the programmer did not think was possible
#define XenInvalidCodePath(msg)	  \
	printf("*** XEN INVALID CODE PATH: %s:%i - %s\n", __FILE__, __LINE__, msg); \
	XenBreak();

////////////////////////////////////////////////////////////////////////////////
// * Fixed Width Primitive Types
//
// Below are ~typedefs~ to be used for fixed width primitives in the engine.
//
// Note that we use ~long long~ for 64 bit types rather than int64_t
//
// This is because under windows ~printf~ for u64 needs to use the format
// specifier "=llu=" or errors will occur (the internal ~printf~ pointer wont be
// advanced far enough, hence subsequent variables will read the wrong memory).
//
// However under unix the ~printf~ specifier "=llu=" implies ~long long~, but
// ~int64_t~ is defined as ~long int~ on 64 bit systems. Hence under unix code
// that is correct on windows produces a compiler warning (which we promote to
// error) if we use ~int64_t~ here, rather than "long long".
//
// This behaviour of producing errors is desirable, since it prevents us from
// introducing windows bugs when developing under unix, but means we cannot use
// int64_t here.
//
// Static assertions are included below to ensure that the types have the
// correct size when we compile on a new platform
typedef float                   r32;
typedef double                  r64;
typedef int8_t                  s8;
typedef int8_t                  s08;
typedef int16_t                 s16;
typedef int32_t                 s32;
typedef long long signed int    s64;
typedef uint8_t                 u8;
typedef uint8_t                 u08;
typedef uint16_t                u16;
typedef uint32_t                u32;
typedef long long unsigned int  u64;
typedef int8_t                  b8;
typedef int32_t                 b32;
typedef unsigned                int uint;
typedef uintptr_t               uptr;
typedef  intptr_t               sptr;

#ifdef XEN_USE_DOUBLE_PRECISION
	typedef double real;
#else
	typedef float  real;
#endif

static_assert(sizeof(u08) == 1, "u08 must be 1 byte");
static_assert(sizeof(s08) == 1, "s08 must be 1 byte");
static_assert(sizeof(u16) == 2, "u16 must be 2 bytes");
static_assert(sizeof(s16) == 2, "u16 must be 2 bytes");
static_assert(sizeof(u32) == 4, "u32 must be 4 bytes");
static_assert(sizeof(s32) == 4, "s32 must be 4 bytes");
static_assert(sizeof(u64) == 8, "u64 must be 8 bytes");
static_assert(sizeof(s64) == 8, "s64 must be 8 bytes");

// ** Real Type Helpers

// User defined suffix for defining a real value, IE: one which will use
// either 32 or 64 bits depending on engine configuration.
// Usage is: ~real x = 1.01_r~
inline constexpr real operator"" _r(long double            val){ return (real)val; }
inline constexpr real operator"" _r(unsigned long long int val){ return (real)val; }

namespace xen {
	// The following values can be used to express the max and min value
	// of the "real" type, regardless of whether we are using 32 or 64 bit
	// floats
	#ifdef XEN_USE_DOUBLE_PRECISION
	static const constexpr real RealMax = DBL_MAX;
	static const constexpr real RealMin = DBL_MIN;
	#else
	static const constexpr real RealMax = FLT_MAX;
	static const constexpr real RealMin = FLT_MIN;
	#endif

	// sqrt function which automatically uses chosen engine precision for reals
	#ifdef XEN_USE_DOUBLE_PRECISION
  inline double sqrt(double val) { return ::sqrt(val);  }
	#else
	inline float  sqrt(float  val) { return sqrtf(val); }
	#endif

	// PI constant stored as either 32 or 64 bits depending on engine precision
	static const constexpr real PI = (real)3.14159265358979323846;

	// Determines if real type represents nan
	inline bool isnan(float a ){ return std::isnan(a); }
	inline bool isnan(double a){ return std::isnan(a); }

	inline u32 round32(real val){ return lround(val); }
	inline u64 round64(real val){ return lround(val); }
}

////////////////////////////////////////////////////////////////////////////////
// * Misc Utility Types and Functions

namespace xen{
	/// \brief Base class for any types which should not be able to be copied
	struct NonCopyable{
		NonCopyable(){}
	private:
		NonCopyable(const NonCopyable& other)            = delete;
		NonCopyable& operator=(const NonCopyable& other) = delete;
	};

	/// \brief Finds the maximum of two or more items
	template<typename T> T max(const T a, const T b){ return a >  b ? a : b; }
	template<typename T> T max(const T a           ){ return a; }
	template<typename T, typename ...T_REST> T max(const T a, const T b, const T c, const T_REST... rest){
		return xen::max(xen::max(a, b), xen::max(c, rest...));
	}

	/// \brief Finds the minimum of two or more items
	template<typename T> T min(const T a, const T b){ return a <= b ? a : b; }
	template<typename T> T min(const T a           ){ return a; }
	template<typename T, typename ...T_REST> T min(const T a, const T b, const T c, const T_REST... rest){
		return xen::min(xen::min(a, b), xen::min(c, rest...));
	}

	/// \brief Swaps two values a and b
	template<typename T>
	void swap(T& a, T& b){
		T tmp = a;
		a     = b;
		b     = tmp;
	}
}

#endif
