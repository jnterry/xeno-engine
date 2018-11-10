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
	/// \brief Represents an array whose size is defined at runtime
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct Array {
		u64 size;
		T*  elements;

		inline T&       operator[](u64 i)       { return elements[i]; }
		inline const T& operator[](u64 i) const { return elements[i]; }

		/// \brief Represents an EmptyArray of type T
		static const Array EmptyArray;
	};
	template<typename T>
	const Array<T> Array<T>::EmptyArray = {0, 0};


	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an array whose size is expected to vary over time
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct StretchyArray : public Array<T> {
		/// \brief Number of elements in allocated array
		u64 capacity;
	};

	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a 2d array whose size may change at runtime
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct Array2d {
		/// \brief The elements of this array, stored in row-major layout
		/// IE: the array  [ a b ]
		//                 [ c d ]
		// Is stored as the array: { a, b, c, d }
		union {
			struct {
				u64 cols; /// \brief Number of columns in the array
				u64 rows; /// \brief Number of rows in the array
			};
			struct {
				u64 width;  /// \brief Width of array, alias for cols
				u64 height; /// \brief Height of array, alias for rows
			};
		};

		T*  elements;

		inline Array<T>       operator[](u64 row)       {
			Array<T> a;
			a.elements = &this->elements[cols * row];
			return a;
		}
		inline const Array<T> operator[](u64 row) const {
			const Array<T> a;
			a.elements = &this->elements[cols * row];
			return a;
		}
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an array whose size is fixed at compile time
	/////////////////////////////////////////////////////////////////////
	template<typename T, u64 T_SIZE>
	struct FixedArray {
		T elements[T_SIZE];

		static const constexpr u64 size = T_SIZE;
		static const constexpr u64 rows = 1;
		static const constexpr u64 cols = T_SIZE;

		FixedArray(){}

		inline T&       operator[](u64 i)       { return elements[i]; }
		inline const T& operator[](u64 i) const { return elements[i]; }

		/// \brief Implicit cast to Array such that functions need only be defined
		/// on standard array type if they should support arrays of arbitrary size
		operator Array<T>()             { return { T_SIZE, this->elements }; }
		operator const Array<T>() const { return { T_SIZE, this->elements }; }
	};
	/*
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

		FixedArray2d(){}

		inline FixedArray<T, T_COLS>       operator[](u64 row)       {
			FixedArray<T, T_COLS> a;
			a.elements = &this->elements[T_COLS * row];
			return a;
		}
		inline const FixedArray<T, T_COLS> operator[](u64 row) const {
			const FixedArray<T, T_COLS> a;
			a.elements = &this->elements[T_COLS * row];
			return a;
		}

		// :TODO: ideally would be indexable by Vec2u, but that introduces a
		// dependency on math module from core module
	};
	*/
}

#endif
