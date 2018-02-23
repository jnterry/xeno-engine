////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declarations of various array types
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_ARRAY_TYPES_HPP
#define XEN_ARRAY_TYPES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an array whose size may change at runtime
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct Array {
	  u64 size;
		T*  elements;

		inline T&       operator[](u64 i)       { return elements[i]; }
		inline const T& operator[](u64 i) const { return elements[i]; }
	};

	// :TODO: Array2d?

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an array whose size is fixed at compile time
	/////////////////////////////////////////////////////////////////////
	template<typename T, u64 T_SIZE>
	struct FixedArray {
		T elements[T_SIZE];

		static const constexpr u64 size = T_SIZE;
		static const constexpr u64 rows = 1;
		static const constexpr u64 cols = T_SIZE;

		inline T&       operator[](u64 i)       { return elements[i]; }
		inline const T& operator[](u64 i) const { return elements[i]; }

		/// \brief Implicit cast to Array such that functions need only be defined
		/// on standard array type if they should support arrays of arbitrary size
		operator Array<T>()             { return { T_SIZE, this->elements }; }
		operator const Array<T>() const { return { T_SIZE, this->elements }; }
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a 2d array whose size is fixed at compile time
	/////////////////////////////////////////////////////////////////////
	template<typename T, u64 T_ROWS, u64 T_COLS>
	struct FixedArray2d {
		/// \brief The elements of this array, stored in row-major layout
		/// IE: the array  [ a b ]
		//                 [ c d ]
		// Is stored as the array: { a, b, c, d }
		T elements[T_ROWS * T_COLS];

		static const constexpr u64 size = T_ROWS * T_COLS;

		inline FixedArray<T, T_COLS>&       operator[](u64 row)       { return elements[T_COLS * row]; }
		inline const FixedArray<T, T_COLS>& operator[](u64 row) const { return elements[T_COLS * row]; }

		// :TODO: ideally would be indexable by Vec2u, but that introduces a
		// dependency on math module from core module
	};
}

#endif
