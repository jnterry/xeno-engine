////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of functions that may operate on xeno engine
/// arrays
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_ARRAY_HPP
#define XEN_CORE_ARRAY_HPP

#include <xen/core/array_types.hpp>

namespace xen{
	/////////////////////////////////////////////////////////////////////
	/// \brief Returns the number of elements in an array
	/// \note  For multidimensional arrays, this is the total number of elements
	///        rather than the size along some dimension
	/////////////////////////////////////////////////////////////////////
  template<typename T, size_t T_SIZE>
  inline constexpr size_t size(const FixedArray<T, T_SIZE>&){ return T_SIZE; }

	//template<typename T, size_t T_ROWS, size_t T_COLS>
	//inline constexpr size_t size(const FixedArray2d<T, T_ROWS, T_COLS>&){ return T_ROWS * T_COLS; }

	// :TODO: findFirst, findLast, etc
	// :TODO: iterator syntax?
}

#endif
