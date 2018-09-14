////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of random number generation
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_RANDOM_CPP
#define XEN_CORE_RANDOM_CPP

#include <stdlib.h>

namespace xen {
	int rand(int low, int high){
		return low + (int)(((float)::rand() / (float)RAND_MAX) * (high - low));
	}

	float randf(float low, float high){
		return low + (((float)::rand() / (float)RAND_MAX) * (high - low));
	}
}

#endif
