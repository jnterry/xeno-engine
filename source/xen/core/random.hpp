////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \file random.hpp
/// \author Jamie Terry
/// \date 2018/01/24
/// \brief Contains various functions for producing random numbers
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_RANDOM_HPP
#define XEN_CORE_RANDOM_HPP

namespace xen{

	/////////////////////////////////////////////////////////////////////
	/// \brief Returns a random integer between low and high inclusive
	/// sampled from a uniform distribution
	/////////////////////////////////////////////////////////////////////
	int rand(int low, int high);


	/////////////////////////////////////////////////////////////////////
	/// \brief Returns a random float between low and high inclusive
	/// sampled from a uniform distribution
	/////////////////////////////////////////////////////////////////////
	float randf(float low, float high);
}

#endif