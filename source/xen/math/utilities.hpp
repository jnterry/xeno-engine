////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains various miscellaneous mathematical utility functions
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_UTILITIES_HPP
#define XEN_MATH_UTILITIES_HPP

namespace xen{
	/// \brief Clamps value to be between low and high
	template<typename T>
	T clamp(T val, T low, T high){
		if(val < low ){ return low;  }
		if(val > high){ return high; }
		return val;
	}

	template<typename T>
	T sign(T a){ return (a < 0) ? -1 : 1; }

	template<typename T>
	T lerp(T low, T high, real frac){
		return low + ((high - low) * frac);
	}
}

#endif
