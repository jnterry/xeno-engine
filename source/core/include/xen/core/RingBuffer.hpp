////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains RingBuffer type and functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_RINGBUFFER_HPP
#define XEN_CORE_RINGBUFFER_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory/utilities.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief A RingBuffer is some buffer of objects to which elements
	/// may be pushed or popped from each end at any time. As such it may be
	/// used for implementing stack and queue like behaviour
	///
	/// In additional the RingBuffer offer's "RollingArray" behaviour, in this
	/// usage the buffer represents some contiguous slice of n elements
	/// taken from some larger virtual array. For example a RingBuffer with
	/// capacity 10 might store the elements 10-19 of the arbitrary length virtual
	/// array. operator[] is defined as per RollingArray semantics, and hence in
	/// this example would be well defined only for the indices in the range 10-19.
	/// Pushing a new element onto the end of the array will cause it to store
	/// the elements 11-20
	/// Note that the size of the slice may be less than the capacity, for example,
	/// the capacity 10 array may just be storing the elements 10-15, and hence
	/// pushing a new element to the back will cause it to store the elements
	/// 10-16 without displacing any other element
	/// This RollingArray behaviour is useful for implementing stream processing
	/// which requires access to some number of elements before and/or after the
	/// examined element
	///
	/// \tparam T_ASSERT_ON_OVERFLOW - if true then XenAssert's will be used
	/// then check pushes do not overflow the buffer and pops do not underflow
	/// the buffer. Note that the use of operator[] is not bound checked in
	/// either case
	/////////////////////////////////////////////////////////////////////
	template<typename T, bool T_ASSERT_ON_OVERFLOW = false>
	struct RingBuffer {
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

	template<typename T, bool T_ASSERT>
	u64 capacity(RingBuffer<T, T_ASSERT>& array) { return array.capacity; }

	template<typename T, bool T_ASSERT>
	u64 size(RingBuffer<T, T_ASSERT>& array) { return array.size; }

	template<typename T, bool T_ASSERT>
	bool isIndexValid(RingBuffer<T, T_ASSERT>& array, u64 index){
		return array.first_index <= index && index < (array.first_index + array.size);
	}

	template<typename T, bool T_ASSERT>
	void clear(RingBuffer<T, T_ASSERT>& array, u64 base_index = 0) {
		array.first_index = base_index;
		array.size        = 0;
	}

	template<typename T, bool T_ASSERT>
	bool hasSpace(RingBuffer<T, T_ASSERT>& array){
		return array.size < array.capacity;
	}

	template<typename T, bool T_ASSERT>
	T* pushFront(RingBuffer<T, T_ASSERT>& array, T value){
		if(T_ASSERT){
			XenAssert(xen::hasSpace(array),
			          "Expected room in order to element to front of ring buffer"
			          );
		}

		array.first_index -= 1;
		array.size        += hasSpace(array);

		if(array.front == 0){
			array.front = array.capacity-1;
		} else {
			--array.front;
		}
		array.elements[array.front] = value;
		return &array.elements[array.front];
	}

	template<typename T, bool T_ASSERT>
	T* pushBack(RingBuffer<T, T_ASSERT>& array, T value){
		if(T_ASSERT){
			XenAssert(xen::hasSpace(array),
			          "Expected room in order to element to back of ring buffer"
			          );
		}

		if(hasSpace(array)){
			array.size += 1;
		} else {
			array.first_index += 1;
			array.front += 1;
			array.front %= array.capacity;
		}

		u64 index = (array.front + array.size - 1) % array.capacity;
		array.elements[index] = value;
		return &array.elements[index];
	}

	template<typename T, bool T_ASSERT>
	bool isEmpty(RingBuffer<T, T_ASSERT>& array) {
		return array.size == 0;
	}


	template<typename T, bool T_ASSERT>
	T popFront(RingBuffer<T, T_ASSERT>& array){
		if(T_ASSERT){
			XenAssert(!xen::isEmpty(array),
			          "Expected RingBuffer to be non-empty when popping front"
			         );
		}

		u64 old_front = array.front;

		array.front += 1;
		array.front %= array.capacity;

		++array.first_index;
		--array.size;

		return array.elements[old_front];
	}

	template<typename T, bool T_ASSERT>
	T popBack(RingBuffer<T, T_ASSERT>& array){
		if(T_ASSERT){
			XenAssert(!xen::isEmpty(array),
			          "Expected RingBuffer to be non-empty when popping back"
			         );
		}

		--array.size;
		return array.elements[(array.front + array.size) % array.capacity];
	}

	template<typename T, bool T_ASSERT>
	T& peakFront(RingBuffer<T, T_ASSERT>& array) {
		if(T_ASSERT){
			XenAssert(!xen::isEmpty(array),
			          "Expected RingBuffer to be non-empty when peaking front"
			         );
		}

		return array.elements[array.front];
	}

	template<typename T, bool T_ASSERT>
	T& peakBack(RingBuffer<T, T_ASSERT>& array) {
		if(T_ASSERT){
			XenAssert(!xen::isEmpty(array),
			          "Expected RingBuffer to be non-empty when peaking back"
			         );
		}
		return array.elements[(array.front + array.size - 1) % array.capacity];
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Removes some element from a RingBuffer replacing it with
	/// the currently last element
	/////////////////////////////////////////////////////////////////////
	template<typename T, bool T_ASSET>
	void removeUnordered(RingBuffer<T, T_ASSET>& array, u64 index){
		u64 last_index = array.first_index + array.size - 1;
		if(index != last_index){
			xen::copyBytes(&array[last_index], &array[index], sizeof(T));
		}
		--array.size;
	}

	//////////////////////////////////////////////////////////////////////
	//
	// Iterator implementation below
	//
	//////////////////////////////////////////////////////////////////////

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents an iterator into a RingBuffer with operator[] providing
	/// access to elements relative to the current position of the iterator
	/////////////////////////////////////////////////////////////////////
	template<typename T, bool T_ASSERT = true>
	struct RingBufferIterator {
		/// \brief The buffer being accessed
		RingBuffer<T, T_ASSERT>* buffer;

		/// \brief The base index being accessed
		u64         index;

		T* operator->(){
			return &(*buffer)[index];
		}

		T& operator*(){
			return (*buffer)[index];
		}

		operator bool() {
			return xen::isIndexValid(*buffer, index);
		}

		/// \brief Retrieves another iterator relative to this one
		RingBufferIterator operator[](s64 delta) {
			return { this->buffer, this->index + delta };
		}

		RingBufferIterator& operator++(){ ++this->index; return *this; }
		RingBufferIterator& operator--(){ ++this->index; return *this; }

		bool operator==(const RingBufferIterator<T, T_ASSERT>& other) const {
			return this->buffer == other.buffer && this->index == other.index;
		}
		bool operator!=(const RingBufferIterator<T, T_ASSERT>& other) const {
			return this->buffer != other.buffer || this->index != other.index;
		}

		s64 operator-(const RingBufferIterator<T, T_ASSERT>& other) const {
			return (s64)this->index - (s64)other.index;
		}
	};

	template<typename T, bool T_ASSERT>
	RingBufferIterator<T, T_ASSERT> iterateFront(RingBuffer<T, T_ASSERT>& buffer){
		return { &buffer, buffer.first_index };
	}

	template<typename T, bool T_ASSERT>
	RingBufferIterator<T, T_ASSERT> iterateBack(RingBuffer<T, T_ASSERT>& buffer){
        return { &buffer, buffer.first_index + buffer.size - 1};
	}

	template<typename T, bool T_ASSERT>
	RingBufferIterator<T, T_ASSERT> iterateAt(RingBuffer<T, T_ASSERT>& buffer, u64 index){
        return { &buffer, index };
	}
}

#endif
