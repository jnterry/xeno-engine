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
#include <xen/core/memory/utilities.hpp>

namespace xen{
	/////////////////////////////////////////////////////////////////////
	/// \brief Returns the number of elements in an array
	/// \note  For multidimensional arrays, this is the total number of elements
	///        rather than the size along some dimension
	/////////////////////////////////////////////////////////////////////
  template<typename T, size_t T_SIZE>
  inline constexpr size_t size(const FixedArray<T, T_SIZE>&){ return T_SIZE; }

	template<typename T>
	inline size_t size(const Array<T>& array){ return array.size; }


	template<typename T, size_t T_SIZE>
  inline constexpr void* clearToZero(FixedArray<T, T_SIZE>& array){
		return clearToZero(array.elements, sizeof(T) * T_SIZE);
	}

	template<typename T>
	inline void* clearToZero(Array<T>& array){
		return clearToZero(array.elements, sizeof(T) * xen::size(array));
	}

	//template<typename T, size_t T_ROWS, size_t T_COLS>
	//inline constexpr size_t size(const FixedArray2d<T, T_ROWS, T_COLS>&){ return T_ROWS * T_COLS; }

	// :TODO: findFirst, findLast, etc
	// :TODO: iterator syntax?
}

#endif
