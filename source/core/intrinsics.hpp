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

#define XenArrayLength(array) (sizeof(array) / sizeof((array)[0]))

/// \brief Triggers break in debugger (or crash if no debugger)
/// \todo :TODO: something better?
#define XenBreak(...) (*(char*)nullptr) = 'a';

#define XenAssert(cond, ...) { if(cond){ XenBreak(__VAR_ARGS__); } }

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
}

#endif
