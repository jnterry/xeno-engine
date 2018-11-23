////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains RollingArray type and functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_ROLLINGARRAY_HPP
#define XEN_CORE_ROLLINGARRAY_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief A RollingArray represents some contiguous slice of n elements
	/// taken from some larger virtual array. For example a RollingArray with
	/// capacity 10 might store the elements 10-19 of the virtual array.
	///
	/// operator[] will thus be well defined for all indices in the range 10-19.
	///
	/// Pushing a new element onto the end of the array will cause it to store
	/// the elements 11-20
	///
	/// Note that the size of the slice may be less than the capacity, for example,
	/// the capacity 10 array may just be storing the elements 10-15, and hence
	/// pushing a new element to the back will cause it to store the elements
	/// 10-16 without displacing any other element
	///
	/// A RollingArray is hence useful for:
	/// - implementing a queue (push to back, pop from front, or vice versa)
	/// - implementing a stack (push and pop from sand end)
	/// - implementing some form of stream processing where access is required to
	///   some number of elements either side of the currently examined element
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct RollingArray {
		typedef T TYPE;

		/// \brief The storage for the elements
		T* elements;

		/// \brief The physical length of the elements array
		u64 capacity;

		/// \brief The index of the last element being stored
		u64 size;

		/// \brief The index of the first element being stored
		u64 first_index;

		/// \brief The element of 'elements' corresponding to the first_index
		u64 front;

		T& operator[](u64 index) {
			u64 true_index = (front + (index - first_index)) % this->capacity;
			return this->elements[true_index];
		}

		const T& operator[](u64 index) const {
			u64 true_index = (front + (index - first_index)) % this->capacity;
			return this->elements[true_index];
		}
	};

	template<typename T>
	u64 capacity(RollingArray<T>& array) { return array.capacity; }

	template<typename T>
	u64 size(RollingArray<T>& array) {
		return array.size;
	}

	template<typename T>
	bool isIndexValid(RollingArray<T>& array, u64 index){
		return array.first_index <= index && index < (array.first_index + array.size);
	}

	template<typename T>
	void clear(RollingArray<T>& array, u64 base_index = 0) {
		array.first_index = base_index;
		array.size        = 0;
	}

	template<typename T>
	bool hasSpace(RollingArray<T>& array){
		return array.size < array.capacity;
	}

	template<typename T>
	T* pushFront(RollingArray<T>& array, T value){
		array.first_index -= 1;
		array.size        += hasSpace(array);

		if(array.front == 0){
			array.front = array.capacity-1;
		} else {
			--array.front;
		}
		array.elements[array.front] = value;
	}

	template<typename T>
	T* pushBack(RollingArray<T>& array, T value){
		if(hasSpace(array)){
			array.size += 1;
		} else {
			array.first_index += 1;
			array.front += 1;
			array.front %= array.capacity;
		}

		array.elements[(array.front + array.size - 1) % array.capacity] = value;
	}


	template<typename T>
	bool isEmpty(RollingArray<T>& array) {
		return array.size == 0;
	}


	template<typename T>
	T popFront(RollingArray<T>& array){
		u64 old_front = array.front;

		array.front += 1;
		array.front %= array.capacity;

		++array.first_index;
		--array.size;

		return array[old_front];
	}

	template<typename T>
	T popBack(RollingArray<T>& array){
		--array.size;
		return array.elements[(array.front + array.size) % array.capacity];
	}

	template<typename T>
	T& peakFront(RollingArray<T>& array) {
		return array.elements[array.front];
	}

	template<typename T>
	T& peakBack(RollingArray<T>& array) {
        return array.elements[(array.front + array.size - 1) % array.capacity];
	}
}

#endif
