////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains various miscellaneous mathematical utility functions
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_UTILITIES_HPP
#define XEN_MATH_UTILITIES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{
	/////////////////////////////////////////////////////////////////////
	/// \brief Clamps a value to be between low and high
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	T clamp(T val, T low, T high){
		if(val < low ){ return low;  }
		if(val > high){ return high; }
		return val;
	}

	/// \brief Wraps a value to be between 0 and high, such that stepping n units
	/// above high wraps around to 0 + n, and stepping n units below 0 loops
	/// around to high - n
	/// \note For positive numbers this is therefore equal to val % high
	template<typename T>
	inline T wrap(T val, T high){
		T wrapped = val % high;
		return wrapped < 0 ? wrapped + high : wrapped;
	}
	template<>
	inline float wrap(float val, float high){
		float wrapped = fmod(val, high);
		return wrapped < 0 ? wrapped + high : wrapped;
	}
	template<>
	inline double wrap(double val, double high){
		double wrapped = fmod(val, high);
		return wrapped < 0 ? wrapped + high : wrapped;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines the sign of an input, returning -1 if negative
	/// and 1 otherwise
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	T sign(T a){ return (a < 0) ? -1 : 1; }

	template<typename T>
	inline T abs(T a){ return ::abs(a); }
	template<>
	inline float  abs(float       a) { return ::fabs(a); }
	inline double abs(double      a) { return ::fabs(a); }
	inline double abs(long double a) { return ::fabs(a); }

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs a linear interpolation between two values
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	T lerp(T low, T high, real frac){
		return low + ((high - low) * frac);
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs an inverse lerp operation, finding the fraction
	/// that in is in the range [low, high]
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	real invLerp(T low, T high, T in){
		return (real)(in - low) / (real)(high - low);
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Maps an input `in` in some range [in_low, in_high] to the
	/// equivalent value in the output range [out_low, out_high]
	/////////////////////////////////////////////////////////////////////
	template<typename T1, typename T2>
	T2 mapToRange(T1 in_low, T1 in_high, T2 out_low, T2 out_high, T1 in){
		return (T2)(lerp(out_low, out_high, invLerp(in_low, in_high, in)));
	}

	/////////////////////////////////////////////////////////////////////
  /// \brief Maps an input `in` in some range [in_low, in_high] to the
	/// equivalent value in the output range [out_low, out_high]
	/// Should the input value be outside of the initial range it is
	/// is first mapped into the initial range
	/////////////////////////////////////////////////////////////////////
	template<typename T1, typename T2>
	T2 mapToRangeClamped(T1 in_low, T1 in_high, T2 out_low, T2 out_high, T1 in){
		return mapToRange(in_low, in_high, out_low, out_high,
		                  clamp(in, in_low, in_high)
		                 );
	}
}

#endif
