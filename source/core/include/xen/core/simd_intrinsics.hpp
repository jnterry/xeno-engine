////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Helper header which includes the intel intrinsics if the engine
/// is being compiled with SSE support
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_SIMD_INTRINSICS_HPP
#define XEN_CORE_SIMD_INTRINSICS_HPP

#if XEN_USE_SSE
#include <mmintrin.h>
#include <xmmintrin.h>
#include <immintrin.h>
#include <emmintrin.h>
#endif

#endif
